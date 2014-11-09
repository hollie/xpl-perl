package xPL::Dock::IAQ2000;

=head1 NAME

xPL::Dock::IAQ2000 - xPL::Dock plugin for a iAQ2000 air quality sensor

=head1 SYNOPSIS

  use xPL::Dock qw/IAQ2000/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds support for an AppliedSensor iAQ2000 sensor

=head1 METHODS

=cut

use 5.014;
use strict;
use warnings;

use English qw/-no_match_vars/;
use xPL::IOHandler;
use xPL::Dock::Plug;
use Data::Dumper;
use Time::HiRes qw(gettimeofday);

our @ISA = qw(xPL::Dock::Plug);
our %EXPORT_TAGS = ( 'all' => [ qw() ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw();
our $VERSION = "1.0";

__PACKAGE__->make_readonly_accessor($_) foreach (qw/baud device/);

=head2 C<getopts( )>

This method returns the L<Getopt::Long> option definition for the
plugin.

=cut

sub getopts {
  my $self = shift;
  $self->{_baud} = 9600;
  return
    (
     'port=s' => \$self->{_device},
     'ultraverbose+' => \$self->{_ultraverbose},
     'logfile=s' => \$self->{_logfile}
    );
}

=head2 C<init(%params)>

=cut

sub init {
  my $self = shift;
  my $xpl = shift;
  my %p = @_;

  $self->required_field($xpl,
                        'device',
                        'The --device parameter is required', 1);
  $self->SUPER::init($xpl, @_);

  # Open the logfile for appending if the user passed one
  if (defined $self->{_logfile}) {
  		open (LOGFILE, ">>" . $self->{_logfile}) || $xpl->ouch("Could not open logfile $self->{_logfile} for appending");
  		print LOGFILE "\n---- iAQ2000 start on " . localtime(time) . " ----\n";		
  }
  
  # Create io handler
  my $io = $self->{_io} =
    xPL::IOHandler->new(xpl => $self->{_xpl}, verbose => $self->verbose,
                        device => $self->{_device},
                        baud => $self->{_baud},
                        reader_callback => sub { $self->device_reader(@_) },
                        input_record_type => 'xPL::IORecord::Simple',
                        ack_timeout_callback => 0.3,
                        output_record_type => 'xPL::IORecord::Simple' );

  

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  # Init the read and write pointer in the plugwise message queue
  $self->{_read_pointer}  = 0;
  $self->{_write_pointer} = 0;
  
  $self->{_previous_value} = 450;
  $self->{_delta} = 10; # only report of the delta is bigger than 10 ppm

  return $self;
}

=head2 C<vendor_id( )>

Defines the vendor ID for this plugin. 

=cut

sub vendor_id {
  'hollie'
}

=head2 C<version( )>

Defines the vendor revision for this plugin. This number will be
reported in the hbeat messages

=cut

sub version {
  '0.1'
}

=head2 C<device_reader( )>

This is the callback that processes output from the Telit module.  It is
responsible for sending out the xPL messages.

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  #print "New message part received from UART: " . $self->_hexdump($new_msg) . "\n";

  my $xpl = $self->xpl;

  # Append newly received string to existing buffer
  $self->{_uart_rx_buffer} .= $new_msg;

  #print "Buffer is now: " . $self->_hexdump($self->{_uart_rx_buffer}) . "\n";

  while ($self->{_uart_rx_buffer} =~ /(.+\r\n)(.*)/s) { # < we need 's' modifier here because we also want to match '\n' with the '.'

      my $frame = $1;
      $self->{_uart_rx_buffer} = $2; # Cut the part we're going to process from the buffer
 	
	print "Found frame '$frame' - remaining: " . $self->_hexdump($self->{_uart_rx_buffer}) . "\n" if ($self->{_ultraverbose});
	
	if (defined $self->{_logfile}) {
		my ($seconds, $useconds) = gettimeofday;
		print LOGFILE "$seconds.$useconds = $frame\n";
	}

    # Processes the received frame
    # and send the required xpl messages
    
    if ($frame =~ /(\d+)\s+/) {
    	my $value = $1;
    	print "Read $value\n" if ($self->{_ultraverbose});
    	my $old_value = $$self{_previous_value};
    	my $delta = $$self{_delta};
    	
    	# Only report if the new value is bigger than delta
    	if ($value < ($old_value - $delta) || $value > ($old_value + $delta) ) {
    		$$self{_previous_value} = $value;
    		
    		my %xplmsg = (
      			message_type => 'xpl-stat',
      		);
    		print "Read AQ value $value\n";
    		$xplmsg{schema} = 'sensor.basic';
			$xplmsg{body} = ['device'  => 'iaq_aww', 'type' => 'generic', 'current' => $value, units => 'ppm'];
			$xpl->send(%xplmsg);
		}
    }
        
    # Send the xpl messages if any
    #if (ref $result eq "ARRAY") {
    #	foreach (@{$result}) {
    #		#print "Sending xpl message\n";
    #		$xpl->send(%{$_});
    #		#print Dumper($_);
    #	}
    #}
    
		
  }
    
  return 'a'; # We need to return something here, but we're not using that value. Fixme or check with beanz!

}



=head2 C<xpl_sensor(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages.  It handles
the incoming sensor.request schema messages.

Supported message commands:
 * requesting status (current switch state, internal clock)
 
=cut
sub xpl_sensor {

  my %p = @_;
  my $msg = $p{message};
  my $self = $p{arguments};
  my $xpl = $self->{_xpl};
  my $packet;

  if ($msg->field('device')) {
    # Commands that target a specific device might need to be sent multiple times
    # if multiple devices are defined
    if (!defined $msg->field('request')){
      return;
    }
    my $request = $msg->field('request');

    foreach my $circle (split /,/, $msg->field('device')) {
      $circle = uc($circle);
 
      if ($request eq 'output' || $request eq 'current') {
	$packet = "0023" . $self->addr_s2l($circle);
      } 
      
      else {
        $xpl->info("internal: Received invalid request '$request'\n");
      }

      # Send the packet to the stick!
      $self->queue_packet_to_stick($packet, "sensor.basic") if (defined $packet);

    }
  }

  return 1;
}

=head2 C<print_stats($name )>

This is a helper function that displays internal status when the option 
'ultraverbose' is set. 

Only required for debugging.

=cut

sub print_stats {
    my ($self, $fname) = @_;

    return if !$self->{_ultraverbose};

    print "------- $fname -------------------------------\n";
    print "UART RX buffer is now '$self->{_uart_rx_buffer}'\n";
    print "++++++++++++++++++++++++++++++++++++++\n";

}

sub _byte_reverse {
	my ($self, $kamstrup) = @_;
	
	my @bytes = split(/(..)/, $kamstrup);
	return join("", reverse @bytes);
}


# Print input string of characters as hex
sub _hexdump {
    my ( $self, $s ) = @_;
    
    my $r = $self->_ascii_to_hex($s);
    
    # Replace unprintable characters with dots
    $s =~ s/[^ -~]/./g;
    
    # Return hex version and cleaned bin version 
    return $r . ' (' . $s . ')';
}

# Convert an ASCII string to the hex representation using two characters in the range 0..F
# to represent a single byte
sub _ascii_to_hex {
	my ($self, $s) = @_;
	return unpack 'H*', $s;	
}

1;
__END__

=head1 EXPORT

None by default.

=head1 SEE ALSO

Project website: http://www.xpl-perl.org.uk/

=head1 AUTHOR

Lieven Hollevoet, E<lt>lieven@lika.beE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2014 by Mark Hindess / Lieven Hollevoet

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
