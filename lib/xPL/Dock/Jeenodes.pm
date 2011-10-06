package xPL::Dock::Jeenodes;

=head1 NAME

xPL::Dock::Jeenodes - xPL::Dock plugin for a Jeelabs Jeenodes

=head1 SYNOPSIS

  use xPL::Dock qw/Jeenodes/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds interfacing to JeeLabs Jeenodes through a Jeenode that is connected to a serial port.
That Jeenode needs to run the following firmware: <tbd>

In this first version, this interface supports reporting measurement values from RF sensors running the 'roomnode' firmware.

=head1 METHODS

=cut

use 5.006;
use strict;
use warnings;

use English qw/-no_match_vars/;
use xPL::IOHandler;
use xPL::Dock::Plug;
use Data::Dumper;

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
  $self->{_baud} = 57600;
  return
    (
     'device=s' => \$self->{_device},
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
                        'The --device parameter is required', 1);
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

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  $self->{_jeenodes}->{connected} = 0;

  # Init
  $self->stick_init();

  return $self;
}

=head2 C<vendor_id( )>

Defines the vendor ID for the plugin. 

=cut

sub vendor_id {
  'hollie'
}

=head2 C<version( )>

Defines the vendor revision for the plugin. This number will be
reported in the hbeat messages

=cut

sub version {
  '0.1'
}

=head2 C<device_reader( )>

This is the callback that processes output from the Jeenode
It is responsible for sending out the xPL messages.

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  #print "New message part received from UART: '$new_msg'\n";

  my $xpl = $self->xpl;

  # Append newly received string to existing buffer
  $self->{_uart_rx_buffer} .= $new_msg;

  print "UART buffer now looks like: '" . $self->{_uart_rx_buffer} . "'\n";

  my $frame;

  # As long we have response packets in the received buffer, process them
  while ($self->{_uart_rx_buffer} =~ /(.+\n)(.*)/s) { # < we need 's' modifier here because we also want to match '\n' with the '.'

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
        $packet = "0017" . "000D6F0000" . $circle . "01";
      }

      elsif ($current eq 'disable') {
        $packet = "0017" . "000D6F0000" . $circle . "00";
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
	$packet = "0023" . "000D6F0000" . $circle;
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
  $self->write_packet_to_stick("?");

  return 1;
}

=head2 C<write_packet_to_stick(payload)>

This function takes the payload for a packet that needs to be sent to the 
JeeNode.

=cut

sub write_packet_to_stick {
  my ($self, $payload) = @_;

  my $packet;
 
  $packet = $payload;                 # Init command

  $self->{_xpl}->info("WR>NODE: $packet\n");

  # Store the last message sent to the UART in a local variable so that we can report it in case of an error
  $self->{_last_pkt_to_uart} = $packet;

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
      message_type => 'xpl-trig',
      schema => 'sensor.basic',
      );


  my $xpl = $self->{_xpl};

  $frame =~ s/(\n)$//; # Strip trailing CRLF

  $xpl->info("RX<NODE: $frame\n");

  # After a command is sent to the stick, we first receive an 'ACK'. This 'ACK' contains a sequence number that we want to track and notifies us of errors.

  $self->{_awaiting_stick_response} = 0;

  #     init response |  seq. nr.     || stick MAC addr || don't care    || network key    || short key
  if ($frame =~ /^aloha/) {
    $self->{_plugwise}->{connected}   = 1;

    $xpl->info("PLUGWISE: Received a valid response to the init request from the Stick. Connected!\n");
    return "no_xpl_message_required";
  }

  print "Frame received: $frame\n";

  #   circle off resp|  seq. nr.     |    | circle MAC
  if ($frame =~/^ROOM(\d+) (\d+) (\d) (\d+) (\d+) (\d)/){
    my $id = $1;
    my $light = $2;
    my $motion = $3;
    my $humi = $4;
    my $temp = $5;
    my $lobat = $6;

    $xpl->info("ROOMNODE[$id] motion detected!\n") if ($motion == 1);
    $xpl->info("ROOMNODE[$id] no motion\n") if ($motion == 0);

    my $current = $motion == 1 ? 'on' : 'off';

    $xplmsg{body} = ['device'  => "room".$id, 'type' => 'motion', 'current' => $current];
 
    return \%xplmsg;

  }

  if ($frame =~/^0000([[:xdigit:]]{4})00DE([[:xdigit:]]{16})$/) {
    my $saddr = $self->addr_l2s($2);
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type};

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
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type};

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
    my $msg_type = $self->{_response_queue}->{hex($1)}->{type};

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
