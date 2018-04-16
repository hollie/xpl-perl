package xPL::Dock::Davis;

=head1 NAME

xPL::Dock::Davis - xPL::Dock plugin for a David ISS console receiver

=head1 SYNOPSIS

  use xPL::Dock qw/Davis/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds reception of Davis ISS weather console messages
via a MoteIno running the VP2 code. See the github account of DeKay for that
code.

=head1 METHODS

=cut

use 5.006;
use strict;
use warnings;

use English qw/-no_match_vars/;
use xPL::IOHandler;
use xPL::Dock::Plug;

use Device::Davis::Strmon;
use Data::Dumper;
use Net::MQTT::Simple;


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
     'port=s' => \$self->{_device},
     'ultraverbose+' => \$self->{_ultraverbose},
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
                        'The --port parameter is required', 1);
  $self->SUPER::init($xpl, @_);

  # Create io handler
  my $io = $self->{_io} =
    xPL::IOHandler->new(xpl => $self->{_xpl}, verbose => $self->verbose,
                        device => $self->{_device},
                        baud => $self->{_baud},
                        reader_callback => sub { $self->device_reader(@_) },
                        input_record_type => 'xPL::IORecord::DoubleLFCRLine',
                        ack_timeout_callback => 3,
                        output_record_type => 'xPL::IORecord::CRLFLine' );

  $self->{_mqtt} = Net::MQTT::Simple->new('nessie.local');
  # Set the state to 'unconnected' to stick, we need to init first!
  $self->{_rfm69}->{connected} = 0;

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  # After opening the serial port we need to wait some time for the bootloader of the moteino to timeout
  # before we send the init command
  sleep(2);
  
  $self->stick_init();
  
  $self->{_last_valid_rx} = 0;

  $self->{_decoder} = Device::Davis::Strmon->new();

  return $self;
}

=head2 C<vendor_id( )>

Defines the vendor ID for the PlugWise plugin.

=cut

sub vendor_id {
  'hollie'
}

=head2 C<version( )>

Defines the vendor revision for the PlugWise plugin. This number will be
reported in the hbeat messages

=cut

sub version {
  '1.0'
}

=head2 C<device_reader( )>

This is the callback that processes output from the RFM69 stick.  It is
responsible for sending out the xPL messages.

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  #print "New message part received from UART: '$new_msg'\n";

  # Attempt to parse
  my $dec = $self->{_decoder};
  # Decoder expects the double LFCR to be present, and our reader strips this.
  # So append it again before feeding the decoder.
  my $output = $dec->decode($new_msg . "\n\r\n\r");
    
  #print Dumper($output);
  
  # Publish to xpl if the CRC is OK
  if ($output->{crc} eq 'ok') {
  	$self->{_last_valid_rx} = time();
  	undef $output->{crc};
  	undef $output->{rawpacket};
  	$self->davis_construct_xpl($output);
  }
  
  return;

#  my $result=$self->plugwise_process_response($frame);
#
#      next if ($result eq "no_xpl_message_required"); # No packet received that needs an xPL message to be sent
#
#      # For other $result values the result already contains the body of the xPL message
#      $xpl->send(%{$result});
#
#  }
#
#  $self->print_stats("end of device reader");
#
#  # Check if we need to send another message to the stick, if there is a message left in the
#  # message queue, send it out
#  $self->process_queue();


  return 'a'; # We need to return something here, but we're not using that value. Fixme or check with beanz!

}

=head2 C<davis_construct_xpl( )>

Create the xpl messages to be sent based on the decoded output of the Davis station.
Returns an reference to an array of xpl messages that need to be sent one by one

=cut

sub davis_construct_xpl( ) {

  my ($self, $data) = @_;

  # The default xpl message is a plugwise.basic trig, can be overwritten when required.
  my %xplmsg = (
      message_type => 'xpl-trig',
      schema => 'sensor.basic',
      );


  my $xpl = $self->{_xpl};

  # Create the various messages based on the packet that was received
  my @msgs;

  #print Dumper $data;

  foreach (keys %{$data}) {
  	next if ($_ eq 'crc');
  	next if ($_ eq 'rawpacket');
  	$self->{_mqtt}->publish( 'weather/' . $_ => $data->{$_}->{'current'});
  };
	
		
    #$xpl->info("DAVIS: Creating xpl packet for $_ with value $data->{$_}\n");
	#$xplmsg{body} = ['device' => $_, $data->{$_}];
	#$xpl->send(%xplmsg);
  


#  #   circle off resp|  seq. nr.     |    | circle MAC
#  if ($frame =~/^0000([[:xdigit:]]{4})00DE([[:xdigit:]]{16})$/) {
#    my $saddr = $self->addr_l2s($2);
#    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "control.basic";
#
#  if ($msg_type eq 'control.basic') {
#    $xplmsg{schema} = 'sensor.basic';
#    $xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'current' => 'LOW'];
#  } else {
#    $xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'onoff' => 'off'];
#  }
#    # Update the response_queue, remove the entry corresponding to this reply
#    delete $self->{_response_queue}->{hex($1)};
#
#    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is OFF\n");
#    return \%xplmsg;
#  }
#
#  #   circle on resp |  seq. nr.     |    | circle MAC
#  if ($frame =~/^0000([[:xdigit:]]{4})00D8([[:xdigit:]]{16})$/) {
#    my $saddr = $self->addr_l2s($2);
#    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "control.basic";
#
#  if ($msg_type eq 'control.basic') {
#    $xplmsg{schema} = 'sensor.basic';
#    $xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'current' => 'HIGH'];
#  } else {
#    $xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'onoff' => 'on'];
#  }
#
#    # Update the response_queue, remove the entry corresponding to this reply
#    delete $self->{_response_queue}->{hex($1)};
#
#    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is ON\n");
#    return \%xplmsg;
#  }
#
#  # Process the response on a powerinfo request
#  # powerinfo resp   |  seq. nr.     ||  Circle MAC    || pulse1        || pulse8        | other stuff we don't care about
#  if ($frame =~/^0013([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})([[:xdigit:]]{4})/) {
#    my $saddr = $self->addr_l2s($2);
#    my $pulse1 = $3;
#    my $pulse8 = $4;
#
#    # Assign the values to the data hash
#    $self->{_plugwise}->{circles}->{$saddr}->{pulse1} = $pulse1;
#    $self->{_plugwise}->{circles}->{$saddr}->{pulse8} = $pulse8;
#
#    # Ensure we have the calibration info before we try to calc the power,
#    # if we don't have it, return an error reponse
#    if (!defined $self->{_plugwise}->{circles}->{$saddr}->{gainA}){
#  $xpl->ouch("Cannot report the power, calibration data not received yet for $saddr\n");
#        $xplmsg{schema} = 'log.basic';
#        $xplmsg{body} = [ 'type' => 'err', 'text' => "Report power failed, calibration data not retrieved yet", 'device' => $saddr ];
#        delete $self->{_response_queue}->{hex($1)};
#
#  return \%xplmsg;
#    }
#
#    # Calculate the live power
#    my ($pow1, $pow8) = $self->calc_live_power($saddr);
#
#    # Update the response_queue, remove the entry corresponding to this reply
#    delete $self->{_response_queue}->{hex($1)};
#
#    # Create the corresponding xPL message
#    $xplmsg{body} = ['device'  => $saddr, 'type' => 'power', 'current' => $pow1/1000, 'current8' => $pow8/1000, 'units' => 'kW'];
#
#    $xpl->info("PLUGWISE: Circle " . $saddr . " live power 1/8 is: $pow1/$pow8 W\n");
#
#    return \%xplmsg;
#  }
#
#  # Process the response on a query known circles command
#  # circle query resp|  seq. nr.     ||  Circle+ MAC   || Circle MAC on  || memory position
#  if ($frame =~/^0019([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{2})$/) {
#    # Store the node in the object
#    if ($3 ne "FFFFFFFFFFFFFFFF") {
#  $self->{_plugwise}->{circles}->{substr($3, -6, 6)} = {}; # Store the last 6 digits of the MAC address for later use
#  # And immediately queue a request for calibration info
#  $self->queue_packet_to_stick("0026".$3, "Request calibration info");
#    }
#
#    # Update the response_queue, remove the entry corresponding to this reply
#    delete $self->{_response_queue}->{hex($1)};
#
#    # Only when we have walked the complete list
#    return "no_xpl_message_required" if ($4 ne sprintf("%02X", $self->{_plugwise}->{list_circles_count} - 1));
#
#    my @xpl_body = ('command' => 'listcircles');
#    my $count = 0;
#    my $device_id;
#
#    foreach $device_id (keys %{$self->{_plugwise}->{circles}}){
#  my $device_string = sprintf("device%02i", $count++);
#  push @xpl_body, ($device_string => $device_id);
#    }
#
#    # Construct the complete xpl message
#    $xplmsg{body} = [@xpl_body];
#    $xplmsg{message_type} = 'xpl-stat';
#
#    return \%xplmsg;
#  }
#
#  # Process the response on a status request
#  # status response  |  seq. nr.     ||  Circle+ MAC   || year,mon, min || curr_log_addr || powerstate
#  if ($frame =~/^0024([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{2})/){
#    my $saddr = $self->addr_l2s($2);
#    my $onoff = $5 eq '00'? 'off' : 'on';
#    my $current = $5 eq '00' ? 'LOW' : 'HIGH';
#    $self->{_plugwise}->{circles}->{$saddr}->{onoff} = $onoff;
#    $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} = (hex($4) - 278528) / 8;
#    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "sensor.basic" ;
#
#    my $circle_date_time = $self->tstamp2time($3);
#
#    $xpl->info("PLUGWISE: Received status reponse for circle $saddr: ($onoff, logaddr=" . $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} . ", datetime=$circle_date_time)\n");
#
#    if ($msg_type eq 'sensor.basic') {
#  $xplmsg{schema} = $msg_type;
#  $xplmsg{body} = ['device' => $saddr, 'type' => 'output', 'current' => $current];
#    } else {
#  $xplmsg{body} = ['device' => $saddr, 'type' => 'output', 'onoff' => $onoff, 'address' => $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr}, 'datetime' => $circle_date_time];
#    }
#    # Update the response_queue, remove the entry corresponding to this reply
#    delete $self->{_response_queue}->{hex($1)};
#
#    return \%xplmsg;
#  }
}


=head2 C<stick_init( )>

This function initializes the connection between the host and the stick. This needs to be called before
any other communication is initiated with the stick.

=cut

sub stick_init {

  my $self = shift();
  $self->write_packet_to_stick("STRMON\r\n");

  return 1;
}

=head2 C<write_packet_to_stick(payload)>

Write a packet to the Davis interface

=cut

sub write_packet_to_stick {
  my ($self, $packet) = @_;

  $self->{_xpl}->info("WR>STICK: $packet\n");

  $self->{_io}->write($packet);

  return 1;
}

sub get_last_valid_rx_time {
	my $self = shift();
	return $self->{_last_valid_rx};
}

sub reset_rx {

	my $self = shift();
	
	$self->{_xpl}->ouch("Resetting RX because no data came in");
	#$self->{_io}->
	
  	$self->{_io} = undef;
  	sleep(1);
    $self->{_io} = xPL::IOHandler->new(xpl => $self->{_xpl}, verbose => $self->verbose,
                        device => $self->{_device},
                        baud => $self->{_baud},
                        reader_callback => sub { $self->device_reader(@_) },
                        input_record_type => 'xPL::IORecord::DoubleLFCRLine',
                        ack_timeout_callback => 3,
                        output_record_type => 'xPL::IORecord::CRLFLine' );
}

1;
__END__

=head1 EXPORT

None by default.

=head1 SEE ALSO

Project website: http://www.xpl-perl.org.uk/

=head1 LIMITATIONS

None known

=head1 AUTHOR

Lieven Hollevoet, E<lt>lieven@lika.beE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2017 by Mark Hindess / Lieven Hollevoet

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
