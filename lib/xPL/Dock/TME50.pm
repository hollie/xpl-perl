package xPL::Dock::TME50;

=head1 NAME

xPL::Dock::TME50 - xPL::Dock plugin for a Telit ME50 wireless mbus receiver

=head1 SYNOPSIS

  use xPL::Dock qw/TME50/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds support for a Telit ME50 wireless mbus receiver.

This module assumes the Telit component is configured to report all fields except 
the wakeup character. 

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
  $self->{_baud} = 19200;
  return
    (
     'telit-tty=s' => \$self->{_device},
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
                        'The --telit-tty parameter is required', 1);
  $self->SUPER::init($xpl, @_);

  # Open the logfile for appending if the user passed one
  if (defined $self->{_logfile}) {
  		open (LOGFILE, ">>" . $self->{_logfile}) || $xpl->ouch("Could not open logfile $self->{_logfile} for appending");
  		print LOGFILE "\n---- Telit software interface start on " . localtime(time) . " ----\n";		
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

  # As long we have response packets in the received buffer, process them
  # First charater tells us the packet length minus one
  # When there is no data, ord returns '0' so we don't keep hangin in this while
  # A full packet is of length REPORTEDLENGTH + 1 (length indicator in front) + 2 (LQI and RSSI)
  while (length($self->{_uart_rx_buffer}) >= (ord($self->{_uart_rx_buffer} ) + 1 + 2)) {
  	
  	# Snip the packet we're going to process from the receive buffer
  	# We need one byte extra because the length byte itself is not counted
  	# And we need two bytes extra because of the link quality and RSSI readings reported by the Telit module.
	my $frame = substr($self->{_uart_rx_buffer}, 0, ord($self->{_uart_rx_buffer})+1+2, "");
	
	# Convert to hex notation
	$frame = $self->_ascii_to_hex($frame);

	
	print "Found frame '$frame' - remaining: " . $self->_hexdump($self->{_uart_rx_buffer}) . "\n" if ($self->{_ultraverbose});
	
	if (defined $self->{_logfile}) {
		my ($seconds, $useconds) = gettimeofday;
		print LOGFILE "$seconds.$useconds = $frame\n";
	}

    # Processes the received frame
    # and send the required xpl messages
    my $result=$self->telit_process_response($frame); 
    
    # Send the xpl messages if any
    if (ref $result eq "ARRAY") {
    	foreach (@{$result}) {
    		#print "Sending xpl message\n";
    		$xpl->send(%{$_});
    		#print Dumper($_);
    	}
    }
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


=head2 C<telit_process_response($frame)>

This function processes a response received from the Telit module

=cut

sub telit_process_response
{
  my ($self, $frame) = @_;

  # The default xpl message is a plugwise.basic trig, can be overwritten when required.
  my %xplmsg = (
      message_type => 'xpl-stat',
      schema => 'sensor.basic',
  );

  my @xpl_messages;
  
  my $xpl = $self->{_xpl};

  $xpl->info("RX<TELIT: $frame\n");

  # Kamstrup compact frame
  if ($frame =~ /21442d2c(\w{12})([[:xdigit:]]{4})(\w{2})([[:xdigit:]]{8})([[:xdigit:]]{4})([[:xdigit:]]{30})([[:xdigit:]]{2})([[:xdigit:]]{2})/ || 
  #            KSTR short|address| don't care   || Acc#|| Session #     || CRC           || Payload        || LQI           || RSSI
  	  $frame =~ /24442d2c(\w{12})([[:xdigit:]]{4})(\w{2})([[:xdigit:]]{8})([[:xdigit:]]{4})([[:xdigit:]]{36})([[:xdigit:]]{2})([[:xdigit:]]{2})/){
  #            KSTR full |address| don't care   || Acc#|| Session #     || CRC           || Payload        || LQI           || RSSI
  
  	my $address        = $1;
  	my $access_number  = hex($3);
  	my $session_number = $4;
  	my $payload        = $6;
  	my $lqi            = hex($7);
  	my $rssi           = hex($8) - 256;
  	
  	my ($info, $volume, $tgt_volume) = $self->_kamstrup_payload_decode($payload);

  	$address = substr($address, 0, 8);
  	$address = $self->_byte_reverse($address);
  	
  	
  	$xpl->info("Received Kamstrup packet from meter $address\n");
  	say "Manufacturer ID: " . $address;
  	say "Access number  : " . $access_number;
  	say "Session #      : " . $session_number;
  	say "Info           : " . $info;
  	say "Volume         : " . $volume;
  	say "Taget Volume   : " . $tgt_volume;
  	say "LQI            : " . $lqi;
  	say "RSSI           : " . $rssi ." dBm";
  	
  	# Prepare the xpl messages reporting info and volumes
  	$xplmsg{body} = ['device'  => $address . "-info", 'type' => 'generic', 'current' => $info & 0x0F];
  	push(@xpl_messages, {%xplmsg});
  	
  	$xplmsg{body} = ['device' => $address . "-volume", 'type' => 'volume', 'current' => $volume];
  	push(@xpl_messages, {%xplmsg});

  	$xplmsg{body} = ['device' => $address . "-tgtvolume", 'type' => 'volume', 'current' => $tgt_volume];
 	push(@xpl_messages, {%xplmsg});
 	
  	$xplmsg{body} = ['device' => $address . "-rssi", 'type' => 'generic', 'current' => $rssi];
 	push(@xpl_messages, {%xplmsg});
 	
  	$xplmsg{body} = ['device' => $address . "-lqi", 'type' => 'generic', 'current' => $lqi];
 	push(@xpl_messages, {%xplmsg});
  	
  	$xplmsg{body} = ['device' => $address . "-sequence", 'type' => 'generic', 'current' => $access_number];
 	push(@xpl_messages, {%xplmsg});

  	return \@xpl_messages;
  };
  


  # We should not get here unless we receive responses that are not implemented...
  $xpl->ouch("Received unknown response: '$frame'");
  return '';

}

# Decode the payload of a Kamstrup transmission
# Returns the value of the info, volume and target volume register
sub _kamstrup_payload_decode {
	my ($self, $payload) = @_;
	
	my ($info, $volume, $tgt_volume);
	
	if ($payload =~ /^79\w{8}(\w{4})(\w{8})(\w{8})/ || 
		$payload =~ /^78\w{6}(\w{4})\w{4}(\w{8})\w{4}(\w{8})/) {
		# Compact frame and full frame
		$info       = hex($self->_byte_reverse($1)) % 16;
		$volume     = hex($self->_byte_reverse($2));
		$tgt_volume = hex($self->_byte_reverse($3));
	} else {
		$self->{_xpl}->ouch("Could not decode payload $payload");
	}
	
	return ($info, $volume, $tgt_volume);
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

Copyright (C) 2005, 2011 by Mark Hindess / Jfn / Lieven Hollevoet

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
