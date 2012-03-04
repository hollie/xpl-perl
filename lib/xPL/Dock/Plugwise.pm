package xPL::Dock::Plugwise;

=head1 NAME

xPL::Dock::Plugwise - xPL::Dock plugin for a Plugwise USB stick

=head1 SYNOPSIS

  use xPL::Dock qw/Plugwise/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds control of a Plugwise network through a Stick.
Compatible with firmware version 2.37 of the Plugwise devices

Current implemented functions using the sensor.basic/control.basic schema:

Switching ON/OFF of circles
Query circles for their status
Query the Circles+ for known circles
Retrieve the live power consumption of a Circle
Readout the historic power consumption of a Circle (1-hour average)

=head1 METHODS

=cut

use 5.006;
use strict;
use warnings;

use English qw/-no_match_vars/;
use xPL::IOHandler;
use xPL::Dock::Plug;
use Digest::CRC qw(crc);
use Data::Dumper;
use Math::Round;

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
  $self->{_baud} = 115200;
  return
    (
     'plugwise-tty=s' => \$self->{_device},
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
                        'The --plugwise-tty parameter is required', 1);
  $self->SUPER::init($xpl, @_);

  # Create io handler
  my $io = $self->{_io} =
    xPL::IOHandler->new(xpl => $self->{_xpl}, verbose => $self->verbose,
                        device => $self->{_device},
                        baud => $self->{_baud},
                        reader_callback => sub { $self->device_reader(@_) },
                        input_record_type => 'xPL::IORecord::Line',
                        ack_timeout_callback => 0.3,
                        output_record_type => 'xPL::IORecord::CRLFLine' );

  # Add a callback to receive incoming plugwise.basic messages
  $xpl->add_xpl_callback(id => 'plugwise-basic', callback => \&xpl_plugwise,
      arguments => $self,
      filter =>
      {
       message_type => 'xpl-cmnd',
       class => 'plugwise',
       class_type => 'basic',
      });
  
  # Add a callback to receive incoming control.basic messages
  $xpl->add_xpl_callback(id => 'control-basic', callback => \&xpl_control,
      arguments => $self,
      filter =>
      {
       message_type => 'xpl-cmnd',
       class => 'control',
       class_type => 'basic',
      });

  # Add a callback to receive incoming sensor.request messages
  $xpl->add_xpl_callback(id => 'sensor-request', callback => \&xpl_sensor,
      arguments => $self,
      filter =>
      {
       message_type => 'xpl-cmnd',
       class => 'sensor',
       class_type => 'request',
      });

  # Set the state to 'unconnected' to stick, we need to init first!
  $self->{_plugwise}->{connected} = 0;

  # Set the number of circles to query for listcircles command
  $self->{_plugwise}->{list_circles_count} = 64;

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  # Init the read and write pointer in the plugwise message queue
  $self->{_read_pointer}  = 0;
  $self->{_write_pointer} = 0;

  $self->stick_init();
  
  $self->{_awaiting_stick_response} = 0;

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

This is the callback that processes output from the Plugwise USB stick.  It is
responsible for sending out the xPL messages.

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  #print "New message part received from UART: '$new_msg'\n";

  my $xpl = $self->xpl;

  # Append newly received string to existing buffer
  $self->{_uart_rx_buffer} .= $new_msg;

  my $frame;

  # As long we have response packets in the received buffer, process them
  while ($self->{_uart_rx_buffer} =~ /(\x05\x05\x03\x03\w+\r\n)(.*)/s) { # < we need 's' modifier here because we also want to match '\n' with the '.'

      $frame = $1;
      $self->{_uart_rx_buffer} = $2; # Cut the part we're going to process from the buffer

      print "Found frame $frame - remaining: $self->{_uart_rx_buffer}\n" if ($self->{_ultraverbose});

      # Processes the received frame, this is done in another routine to keep the protocol section separated from the hardware interfacing code
      # This function will return either a message that tells not to send and xPL message, or an actual xPL message to be sent
      my $result=$self->plugwise_process_response($frame); 

      next if ($result eq "no_xpl_message_required"); # No packet received that needs an xPL message to be sent

      # For other $result values the result already contains the body of the xPL message
      $xpl->send(%{$result});

  }

  $self->print_stats("end of device reader");

  # Check if we need to send another message to the stick, if there is a message left in the 
  # message queue, send it out
  $self->process_queue();

  
  return 'a'; # We need to return something here, but we're not using that value. Fixme or check with beanz!

}


=head2 C<xpl_plugwise(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages using the schema
plugwise.basic.

Supported messages commands are:
 * listcircles : get a list of connected Circles, respond with their ID's

Supported message commands with a device specifier are:
 * on     : switch a circle on
 * off    : switch a circle off
 * status : request the current switch state, internal clock, live power consumption
 * livepower: request the current power measured by the Circle
 * history: request the energy consumption for a specific logaddress
 
=cut

sub xpl_plugwise {

  my %p = @_;
  my $msg = $p{message};
  my $self = $p{arguments};
  my $xpl = $self->{_xpl};
  my $packet;

  my $command = lc($msg->field('command'));

  # Commands that have no specific device
  if ($command eq 'listcircles') {
     $self->query_connected_circles();
     return 1;
  }

  if ($msg->field('device')) {
    # Commands that target a specific device might need to be sent multiple times
    # if multiple devices are defined
    foreach my $circle (split /,/, $msg->field('device')) {
      $circle = uc($circle);
 
     if ($command eq 'on') {
        $packet = "0017" . $self->addr_s2l($circle) . "01";
      }

      elsif ($command eq 'off') {
        $packet = "0017" . $self->addr_s2l($circle) . "00";
      }

      elsif ($command eq 'status') {
        $packet = "0023" . $self->addr_s2l($circle);
      }

      elsif ($command eq 'livepower') {
        # Ensure we have the calibration readings before we send the read command 
        # because the processing of the response of the read command required the 
        # calibration readings output to calculate the actual power
        if (!defined($self->{_plugwise}->{circles}->{$circle}->{offruis})) {
            my $longaddr = $self->addr_s2l($circle);
            $self->queue_packet_to_stick("0026". $longaddr, "Request calibration info");
        }
        $packet = "0012" . $self->addr_s2l($circle);
      }

      elsif ($command eq 'history') {
        # Ensure we have the calibration readings before we send the read command 
        # because the processing of the response of the read command required the 
        # calibration readings output to calculate the actual power
        if (!defined($self->{_plugwise}->{circles}->{$circle}->{offruis})) {
            my $longaddr = $self->addr_s2l($circle);
            $self->queue_packet_to_stick("0026". $longaddr, "Request calibration info");
        }
        my $address = $msg->field('address') * 8 + 278528;
        $packet = "0048" . $self->addr_s2l($circle) . sprintf("%08X", $address);
      } 

      else {
        $xpl->info("internal: Received invalid command '$command'\n");
      }

      # Send the packet to the stick!
      $self->queue_packet_to_stick($packet, "plugwise.basic") if (defined $packet);

    }
  }

  return 1;
}

=head2 C<xpl_control(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages.  It handles
the incoming control.basic schema messages.

Supported message commands:
 * enabling/disabling Circles
 
=cut
sub xpl_control {

  my %p = @_;
  my $msg = $p{message};
  my $self = $p{arguments};
  my $xpl = $self->{_xpl};
  my $packet;

  my $current = lc($msg->field('current'));

  if ($msg->field('device')) {
    # Commands that target a specific device might need to be sent multiple times
    # if multiple devices are defined
    foreach my $circle (split /,/, $msg->field('device')) {
      $circle = uc($circle);
 
     if ($current eq 'enable') {
        $packet = "0017" . $self->addr_s2l($circle) . "01";
      }

      elsif ($current eq 'disable') {
        $packet = "0017" . $self->addr_s2l($circle) . "00";
      }

      else {
        $xpl->info("internal: Received invalid command '$current'\n");
      }

      # Send the packet to the stick!
      $self->queue_packet_to_stick($packet, "control.basic") if (defined $packet);

    }
  }

  return 1;
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

=head2 C<stick_init( )>

This function initializes the connection between the host and the stick. This needs to be called before 
any other communication is initiated with the stick.

=cut

sub stick_init {

  my $self = shift();
  $self->write_packet_to_stick("000A");

  return 1;
}

=head2 C<write_packet_to_stick(payload)>

This function takes the payload for a packet that needs to be sent to the 
USB stick, adds header and CRC and sends it to the stick.
It also pushes a command on the 'packets_in_progress' list so that this 
module can keep track of it when feedback is received.

=cut

sub write_packet_to_stick {
  my ($self, $payload) = @_;

  my $packet;
 
  $packet = $payload;                 # Init command
  $packet .= plugwise_crc ($packet);  # Add CRC

  $self->{_xpl}->info("WR>STICK: $packet\n");

  # Store the last message sent to the UART in a local variable so that we can report it in case of an error
  $self->{_last_pkt_to_uart} = $packet;

  $packet = "\05\05\03\03" . $packet; # Add header (crlf termination is handled by io handle)

  $self->{_io}->write($packet);

  # Keep track of the fact that we sent a packet, we should now get a response
  $self->{_awaiting_stick_response} = 1;

  return 1;
}

=head2 C<queue_packet_to_stick($packet, $description_string)>

This function is the default function to call in case a message needs to be sent to the USB stick.
It pushes the message on the message queue where it will be sent from when
all previous messages are sent and a response has been received for.

This is required because otherwise the USB stick can be flooded with commands.

=cut

sub queue_packet_to_stick {
  my ($self, $packet, $description) = @_;

  my $writeptr = $self->{_write_pointer}++;
  my $readptr  = $self->{_read_pointer};

  $self->{_msg_queue}->{$writeptr}->{packet} = $packet;
  $self->{_msg_queue}->{$writeptr}->{type} = $description;
 
  $self->print_stats("queue_to_stick");

  $self->process_queue();

  return;
}

=head2 C<process_queue( )>

This function is the default function to call in case a message needs to be sent to the USB stick.
It pushes the message on the message queue where it will be sent from when
all previous messages are sent and a response has been received for.

This is required because otherwise the USB stick can be flooded with commands.

=cut
sub process_queue {
  my ($self) = @_;

  # If the queue it empty, return
  if ((scalar keys %{$self->{_msg_queue}}) == 0) {
      print "Processing empty queue, returning...\n" if ($self->{_ultraverbose});
      return;
  }

  my $readptr  = $self->{_read_pointer};
  
  # If we're no longer waiting for a response and the response queue is empty, the it is OK to send the next packet
  if (!$self->{_awaiting_stick_response}) {
      $self->write_packet_to_stick($self->{_msg_queue}->{$readptr}->{packet});
      # We need to keep track of the type of message so that we can determine what type of response we need to send later
      $self->{_response_queue}->{last_type} = $self->{_msg_queue}->{$readptr}->{type};
      delete $self->{_msg_queue}->{$readptr};

      $self->{_read_pointer}++;
  }  
  else {
      print "Process_queue: seems we're waiting for a response from a previous command (rd: $readptr)\n" if ($self->{_ultraverbose}); 
  }

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
    print "Message queue is:\n";
    print Dumper($self->{_msg_queue});
    print "Response queue is: \n";
    print Dumper($self->{_response_queue});
    print "Plugwise connected status is $self->{_plugwise}->{connected}\n";
    print "UART RX buffer is now '$self->{_uart_rx_buffer}'\n";
    print "Pointers: RD: $self->{_read_pointer}\n";
    print "Awaiting_response: $self->{_awaiting_stick_response}\n";
    print "++++++++++++++++++++++++++++++++++++++\n";

}

=head2 C<plugwise_crc($string)>

This is a helper function that returns the CRC for communication with the USB stick.

TODO: convert this to a real function of the class.

=cut

sub plugwise_crc
{
  sprintf ("%04X", crc($_[0], 16, 0, 0, 0, 0x1021, 0));
}

=head2 C<plugwise_process_response($frame)>

This function processes a response received from the USB stick.

In a first step, the ACK response from the stick is handled. This means that the
communication sequence number is captured, and a new entry is made in the response queue.

Second step, if we receive an error response from the stick, generate a
notification message using the log.basic schema

=cut

sub plugwise_process_response
{
  my ($self, $frame) = @_;

  #my @xpl_body;

  # The default xpl message is a plugwise.basic trig, can be overwritten when required.
  my %xplmsg = (
      message_type => 'xpl-stat',
      schema => 'plugwise.basic',
      );


  my $xpl = $self->{_xpl};

  $frame =~ s/(\n|.)*\x05\x05\x03\x03//g; # Strip header
  $frame =~ s/(\r\n)$//; # Strip trailing CRLF

  $xpl->info("RX<STICK: $frame\n");

  # Check if the CRC matches
  if (! (plugwise_crc( substr($frame, 0, -4)) eq substr($frame, -4, 4))) {
      # Send out notification...
      $xpl->ouch("PLUGWISE: received a frame with an invalid CRC");
      $xplmsg{schema} = 'log.basic';
      $xplmsg{body} = [ 'type' => 'err', 'text' => "Received frame with invalid CRC", 'code' => $frame ];
      return "";
  }

  # Strip CRC, we already know it is correct
  $frame =~ s/(.{4}$)//;

  # After a command is sent to the stick, we first receive an 'ACK'. This 'ACK' contains a sequence number that we want to track and notifies us of errors.
  if ($frame =~ /^0000([[:xdigit:]]{4})([[:xdigit:]]{4})$/) {
  #      ack          |  seq. nr.     || response code |
    if ($2 eq "00C1") {
      $self->{_response_queue}->{hex($1)}->{received_ok} = 1;
      $self->{_response_queue}->{hex($1)}->{type} = $self->{_response_queue}->{last_type};
      return "no_xpl_message_required"; # We received ACK from stick, we should not send an xPL message out for this response
    } elsif ($2 eq "00C2"){
      # We sometimes get this reponse on the initial init request, re-init in this case
      $self->write_packet_to_stick("000A");
      return "no_xpl_message_required";
    } else {  
      $xpl->ouch("Received response code with error: $frame\n");
      $xplmsg{schema} = 'log.basic';
      $xplmsg{body} = [ 'type' => 'err', 'text' => "Received error response", 'code' => $self->{_last_pkt_to_uart} . ":" . $2 ];
      delete $self->{_response_queue}->{hex($1)};
      $self->{_awaiting_stick_response} = 0;
    
      return \%xplmsg;

    }
  }

  $self->{_awaiting_stick_response} = 0;

  #     init response |  seq. nr.     || stick MAC addr || don't care    || network key    || short key
  if ($frame =~ /^0011([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})/) {
    # Extract info
    $self->{_plugwise}->{stick_MAC}   = substr($2, -6, 6);
    $self->{_plugwise}->{network_key} = $4;
    $self->{_plugwise}->{short_key}   = $5;
    $self->{_plugwise}->{connected}   = 1;

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    $xpl->info("PLUGWISE: Received a valid response to the init request from the Stick. Connected!\n");
    return "no_xpl_message_required";
  }

  #   circle off resp|  seq. nr.     |    | circle MAC
  if ($frame =~/^0000([[:xdigit:]]{4})00DE([[:xdigit:]]{16})$/) {
    my $saddr = $self->addr_l2s($2);
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "control.basic";

	if ($msg_type eq 'control.basic') {
		$xplmsg{schema} = 'sensor.basic';
		$xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'current' => 'LOW'];
	} else {
		$xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'onoff' => 'off'];
	}
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is OFF\n");
    return \%xplmsg;      
  }

  #   circle on resp |  seq. nr.     |    | circle MAC
  if ($frame =~/^0000([[:xdigit:]]{4})00D8([[:xdigit:]]{16})$/) {
    my $saddr = $self->addr_l2s($2);
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "control.basic";

	if ($msg_type eq 'control.basic') {
		$xplmsg{schema} = 'sensor.basic';
		$xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'current' => 'HIGH'];
	} else {
		$xplmsg{body} = ['device'  => $saddr, 'type' => 'output', 'onoff' => 'on'];
	}
	
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is ON\n");
    return \%xplmsg;      
  }

  # Process the response on a powerinfo request
  # powerinfo resp   |  seq. nr.     ||  Circle MAC    || pulse1        || pulse8        | other stuff we don't care about
  if ($frame =~/^0013([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})([[:xdigit:]]{4})/) {
    my $saddr = $self->addr_l2s($2);
    my $pulse1 = $3;
    my $pulse8 = $4;

    # Assign the values to the data hash
    $self->{_plugwise}->{circles}->{$saddr}->{pulse1} = $pulse1;
    $self->{_plugwise}->{circles}->{$saddr}->{pulse8} = $pulse8;

    # Ensure we have the calibration info before we try to calc the power, 
    # if we don't have it, return an error reponse
    if (!defined $self->{_plugwise}->{circles}->{$saddr}->{gainA}){
	$xpl->ouch("Cannot report the power, calibration data not received yet for $saddr\n");
        $xplmsg{schema} = 'log.basic';
        $xplmsg{body} = [ 'type' => 'err', 'text' => "Report power failed, calibration data not retrieved yet", 'device' => $saddr ];
        delete $self->{_response_queue}->{hex($1)};

	return \%xplmsg;
    }

    # Calculate the live power
    my ($pow1, $pow8) = $self->calc_live_power($saddr);
    
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    # Create the corresponding xPL message
    $xplmsg{body} = ['device'  => $saddr, 'type' => 'power', 'current' => $pow1/1000, 'current8' => $pow8/1000, 'units' => 'kW'];

    $xpl->info("PLUGWISE: Circle " . $saddr . " live power 1/8 is: $pow1/$pow8 W\n");

    return \%xplmsg;
  }

  # Process the response on a query known circles command
  # circle query resp|  seq. nr.     ||  Circle+ MAC   || Circle MAC on  || memory position
  if ($frame =~/^0019([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{2})$/) {
    # Store the node in the object
    if ($3 ne "FFFFFFFFFFFFFFFF") {
	$self->{_plugwise}->{circles}->{substr($3, -6, 6)} = {}; # Store the last 6 digits of the MAC address for later use
	# And immediately queue a request for calibration info
	$self->queue_packet_to_stick("0026".$3, "Request calibration info");
    }
      
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    # Only when we have walked the complete list
    return "no_xpl_message_required" if ($4 ne sprintf("%02X", $self->{_plugwise}->{list_circles_count} - 1));

    my @xpl_body = ('command' => 'listcircles');
    my $count = 0;
    my $device_id;

    foreach $device_id (keys %{$self->{_plugwise}->{circles}}){
	my $device_string = sprintf("device%02i", $count++);
	push @xpl_body, ($device_string => $device_id);
    }

    # Construct the complete xpl message
    $xplmsg{body} = [@xpl_body];
    $xplmsg{message_type} = 'xpl-stat';

    return \%xplmsg;
  }

  # Process the response on a status request
  # status response  |  seq. nr.     ||  Circle+ MAC   || year,mon, min || curr_log_addr || powerstate
  if ($frame =~/^0024([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{2})/){
    my $saddr = $self->addr_l2s($2);
    my $onoff = $5 eq '00'? 'off' : 'on';
    my $current = $5 eq '00' ? 'LOW' : 'HIGH';
    $self->{_plugwise}->{circles}->{$saddr}->{onoff} = $onoff;
    $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} = (hex($4) - 278528) / 8;
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type} || "sensor.basic" ;

    my $circle_date_time = $self->tstamp2time($3);

    $xpl->info("PLUGWISE: Received status reponse for circle $saddr: ($onoff, logaddr=" . $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} . ", datetime=$circle_date_time)\n");

    if ($msg_type eq 'sensor.basic') {
	$xplmsg{schema} = $msg_type;
	$xplmsg{body} = ['device' => $saddr, 'type' => 'output', 'current' => $current];
    } else {
	$xplmsg{body} = ['device' => $saddr, 'type' => 'output', 'onoff' => $onoff, 'address' => $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr}, 'datetime' => $circle_date_time];
    }
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    return \%xplmsg;
  }

  # Process the response on a calibration request
  if ($frame =~/^0027([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{8})$/){
  # calibration resp |  seq. nr.     ||  Circle+ MAC   || gainA         || gainB         || offtot        || offruis
    #print "Received for $2 calibration response!\n";
    my $saddr = $self->addr_l2s($2);
    #print "Short address  = $saddr\n";
    $xpl->info("PLUGWISE: Received calibration reponse for circle $saddr\n");

    $self->{_plugwise}->{circles}->{$saddr}->{gainA}   = $self->hex2float($3);
    $self->{_plugwise}->{circles}->{$saddr}->{gainB}   = $self->hex2float($4);
    $self->{_plugwise}->{circles}->{$saddr}->{offtot}  = $self->hex2float($5);
    $self->{_plugwise}->{circles}->{$saddr}->{offruis} = $self->hex2float($6);

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    return "no_xpl_message_required";      
  }

  # Process the response on a historic buffer readout
  if ($frame =~/^0049([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{16})([[:xdigit:]]{8})$/){
  # history resp     |  seq. nr.     ||  Circle+ MAC   || info 1         || info 2         || info 3         || info 4         || address
    my $s_id     = $self->addr_l2s($2);
    my $log_addr = (hex($7) - 278528) / 8 ;  
    #print "Received history response for $2 and address $log_addr!\n";

    # Assign the values to the data hash
    $self->{_plugwise}->{circles}->{$s_id}->{history}->{logaddress} = $log_addr;
    $self->{_plugwise}->{circles}->{$s_id}->{history}->{info1} = $3;
    $self->{_plugwise}->{circles}->{$s_id}->{history}->{info2} = $4;
    $self->{_plugwise}->{circles}->{$s_id}->{history}->{info3} = $5;
    $self->{_plugwise}->{circles}->{$s_id}->{history}->{info4} = $6;

    # Ensure we have the calibration info before we try to calc the power, 
    # if we don't have it, return an error reponse
    if (!defined $self->{_plugwise}->{circles}->{$s_id}->{gainA}){
	$xpl->ouch("Cannot report the power, calibration data not received yet for $s_id\n");
        $xplmsg{schema} = 'log.basic';
        $xplmsg{body} = [ 'type' => 'err', 'text' => "Report power failed, calibration data not retrieved yet", 'device' => $s_id ];
        delete $self->{_response_queue}->{hex($1)};

	return \%xplmsg;
    }
    my ($tstamp, $energy) = $self->report_history($s_id);

    $xplmsg{body} = ['device' => $s_id, 'type' => 'energy', 'current' => $energy, 'units' => 'kWh', 'datetime' => $tstamp];
	
	$xpl->info("PLUGWISE: Historic energy for $s_id"."[$log_addr] is $energy kWh on $tstamp\n");
	
    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    return \%xplmsg;
  }

  # We should not get here unless we receive responses that are not implemented...
  $xpl->ouch("Received unknown response: '$frame'");
  return "no_xpl_message_required";

}

sub hex2float {
    my ($self, $hexstr) = @_;

    my $floater = unpack('f', reverse pack('H*', $hexstr));

    return $floater;
}

sub pulsecorrection {
    my ($self, $id, $pulses) = @_;

    # Get the calibration values for the circle
    my $offnoise = $self->{_plugwise}->{circles}->{$id}->{offruis};
    my $offtot   = $self->{_plugwise}->{circles}->{$id}->{offtot};
    my $gainA    = $self->{_plugwise}->{circles}->{$id}->{gainA};
    my $gainB    = $self->{_plugwise}->{circles}->{$id}->{gainB};

    # Correct the pulses with the calibration data
    my $out = (($pulses + $offnoise) ^ 2) * $gainB + (($pulses + $offnoise ) * $gainA ) + $offtot;
    
    # Never report negative values, can happen with really small values
    $out = 0 if ($out < 0);
    
    return $out;

}
sub calc_live_power {
    my ($self, $id) = @_;

    #my ($pulse1, $pulse8) = $self->pulsecorrection($id);
    my $pulse1 = $self->pulsecorrection($id, hex($self->{_plugwise}->{circles}->{$id}->{pulse1}));
    my $pulse8 = $self->pulsecorrection($id, hex($self->{_plugwise}->{circles}->{$id}->{pulse8})/8);
    
    my $live1 = $pulse1 * 1000 / 468.9385193;
    my $live8 = $pulse8 * 1000 / 468.9385193;

    # Round
    $live1 = round($live1);
    $live8 = round($live8);

    return ($live1, $live8);

}

sub report_history {
    my ($self, $id) = @_;

    # Get the first data entry
    my $data = $self->{_plugwise}->{circles}->{$id}->{history}->{info1};

	my $energy = 0;
	my $tstamp = 0;
	
    if ($data =~ /^([[:xdigit:]]{8})([[:xdigit:]]{8})$/){
        # Calculate kWh
        my $corrected_pulses = $self->pulsecorrection($id, hex($2));
        $energy = $corrected_pulses / 3600 / 468.9385193;
        $tstamp = $self->tstamp2time($1);
    	
    	# Round to 1 Wh
    	$energy = round($energy);
    	
        #print "info1 date: $tstamp, energy $energy kWh\n";
    }
    
    return ($tstamp, $energy);
    
}

# Convert the packed time to xPL datetime
sub tstamp2time {
    my ($self, $tstamp) = @_;
    
    # Return empty time on empty timestamp
    return "000000000000" if ($tstamp eq "FFFFFFFF");

    # Convert
    if ($tstamp =~ /([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{4})/){
        my $circle_date    = sprintf("%04i%02i%02i", 2000+hex($1), hex($2), int(hex($3)/60/24)+1); 
        my $circle_time    = hex($3) % (60*24);
        my $circle_hours   = int($circle_time/60);
        my $circle_minutes = $circle_time % 60;
        $circle_time       = sprintf("%02i%02i", $circle_hours, $circle_minutes);
        return $circle_date . $circle_time;
    } else {
        return "000000000000";
    }
}


# Interrogate the network coordinator (Circle+) for all connected Circles
# This sub will generate the requests, and then the response parser function 
# will generate a hash with all known circles
# When a circle is detected, a calibration request is sent to ge the relevant info
# required to calculate the power information.
# Circle info goes into a global hash like this:
#   $object->{_plugwise}->{circles}
#      A single circle entry contains the short id and the following info:
#         short_id => { gainA   => xxx,
#                       gainB   => xxx,
#                       offtot  => xxx,
#                       offruis => xxx }  
sub query_connected_circles {

    my ($self) = @_;

    # In this code we will scan all connected circles to be able to add them to the $self->{_plugwise}->{circles} hash
    my $index = 0;

    # Interrogate the Circle+ and add its info into the circles hash
    $self->{_plugwise}->{coordinator_MAC} = $self->addr_l2s($self->{_plugwise}->{network_key});
    $self->{_plugwise}->{circles} = {}; # Reset known circles hash
    $self->{_plugwise}->{circles}->{$self->{_plugwise}->{coordinator_MAC}} = {}; # Add entry for Circle+
    $self->queue_packet_to_stick("0026".$self->addr_s2l($self->{_plugwise}->{coordinator_MAC}), "Calibration request for Circle+");

    # Interrogate the first x connected devices
    while ($index < $self->{_plugwise}->{list_circles_count}) {
	my $strindex = sprintf("%02X", $index++);
	my $packet   = "0018" . $self->addr_s2l($self->{_plugwise}->{coordinator_MAC}) . $strindex;
	$self->queue_packet_to_stick($packet, "Query connected device $strindex");
    }

    return;
}

# Convert the long Circle address notation to short
sub addr_l2s {
    my ($self,$address) = @_;
    my $saddr = substr($address, -8, 8);
    # We will return at least 6 bytes, more if required
    # This is to keep compatibility with existing code that only supports 6 byte short addresses
    return sprintf("%06X", hex($saddr));
}

# Convert the short Circle address notation to long
sub addr_s2l {
    my ($self,$address) = @_;
    
    return "000D6F00" . sprintf("%08X", hex($address));
}

1;
__END__

=head1 EXPORT

None by default.

=head1 SEE ALSO

Project website: http://www.xpl-perl.org.uk/

=head1 LIMITATIONS

When interrogating the Circle+ for known devices, only the first 64 devices 
are requested. It is not clear if the firmware supports more, as there is 
no official firmware specification.
The number of devices requested can be increased by adapting 
the 'list_circles_count' setting in the init function.

=head1 AUTHOR

Jfn, E<lt>pe1pqf@REMOVE_THISzonnet.nlE<gt>
Lieven Hollevoet, E<lt>lieven@lika.beE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2011 by Mark Hindess / Jfn / Lieven Hollevoet

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
