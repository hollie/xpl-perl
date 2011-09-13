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

Current implemented functions:

Switching ON/OFF of circles
Query circles for their status
Query the Circles+ for known circles

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

our @ISA = qw(xPL::Dock::Plug);
our %EXPORT_TAGS = ( 'all' => [ qw() ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw();
our $VERSION = qw/$Revision$/[1];

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

  # Override the default 'bnz' vendor id
  $xpl->{_vendor_id} = "hollie";

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

  # Add a callback to receive incoming xPL messages
  $xpl->add_xpl_callback(id => 'xpl-plug', callback => \&xpl_plug,
                         arguments => $self,
                         filter =>
                         {
                          message_type => 'xpl-cmnd',
                          class => 'plugwise',
                          class_type => 'basic',
                         });

  # Set the state to 'unconnected' to stick, we need to init first!
  $self->{_plugwise}->{connected} = 0;

  # Set the number of circles to query for listcircles command
  $self->{_plugwise}->{list_circles_count} = 8;

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  # Init the read and write pointer in the plugwise message queue
  $self->{_read_pointer}  = 0;
  $self->{_write_pointer} = 0;

  $self->stick_init();

  $self->{_awaiting_stick_response} = 0;

  return $self;
}

=head2 C<device_reader()>

This is the callback that processes output from the Plugwise USB stick.  It is
responsible for sending out the xPL messages.

Current implementation involves sending out confirmation messages about the
status of Circles that are switched on/off

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

      # Processes the received frame, this is done in another routine to split the Plugwise protocol implementation and the xPL packet generation
      my $result=$self->plugwise_process_response($frame); 

      #$self->print_stats("device_reader");

      next if ($result eq "no_xpl_message_required"); # No packet received that needs an xPL message to be sent

      # For other $result values the result already contains the body of the xPL message
      my %xplmsg = (
	  message_type => 'xpl-trig',
          schema => 'plugwise.basic',
          body => @{$result},
      );

      $xpl->send(%xplmsg);

      my $function_code = 0x66; #hex2int (substr($result, 0, 4)); # Function code

      # elsif ($function_code == 0x24) { # Message is response to status info request
      #     $xplmsg{'body'}{'command'} = 'status';
      #     $xplmsg{'body'}{'device'}  = substr($result, 14, 6); # Macaddress
      #     $xplmsg{'body'}{'abshour'} = substr($result, 20, 8); # Hours passed since 1-6-2007 00:00
      #     $xplmsg{'body'}{'lastlog'} = substr($result, 28, 8); # Last log address available
      #     $xplmsg{'body'}{'onoff'}   = hex2int(substr($result, 36,2)) ? 'on' : 'off';
      #     $xplmsg{'body'}{'hwver'}   = substr($result, 40, 12); # HW version of the circle
      # }

      # elsif ($function_code == 0x13) { # Message is response to power info request 
      #     $xplmsg{'body'}{'command'}   = 'powerinfo';
      #     $xplmsg{'body'}{'device'}    = substr($result, 14, 6); # Macaddress
      #     $xplmsg{'body'}{'pulse8sec'} = substr($result, 20, 4); # Pulse information of 8 seconds reading
      #     $xplmsg{'body'}{'pulse1sec'} = substr($result, 24, 4); # Pulse information of 1 second reading
      #     $xplmsg{'body'}{'unknown'}   = substr($result, 28, 8); # Yet unknown what this means
      # }

      # elsif ($function_code == 0x49) { # Message is response to power buffer request
      #     $xplmsg{'body'}{'command'}     = 'powerbuf';
      #     $xplmsg{'body'}{'device'}      = substr($result, 14, 6); # Macaddress
      #     $xplmsg{'body'}{'firstbuf'}    = substr($result, 20, 8); # Hour of 1st buffer (abshour format)
      #     $xplmsg{'body'}{'firstpulse'}  = substr($result, 28, 8); # Usage in pulses 1st hour
      #     $xplmsg{'body'}{'secondbuf'}   = substr($result, 36, 8); # Hour of 2nd buffer (abshour format)
      #     $xplmsg{'body'}{'secondpulse'} = substr($result, 44, 8); # Usage in pulses 2nd hour
      #     $xplmsg{'body'}{'thirdbuf'}    = substr($result, 52, 8); # Hour of 3rd buffer (abshour format)
      #     $xplmsg{'body'}{'thirdpulse'}  = substr($result, 60, 8); # Usage in pulses 3rd hour
      #     $xplmsg{'body'}{'fourthbuf'}   = substr($result, 68, 8); # Hour of 4th buffer (abshour format)
      #     $xplmsg{'body'}{'fourthpulse'} = substr($result, 76, 8); # Usage in pulses 4th hour
      #     $xplmsg{'body'}{'curlogaddr'}  = substr($result, 84, 8); # Log address of current buffer
      # }

  }

  $self->print_stats("end of device reader");

  # Check if we need to send another message to the stick, if there is a message left in the 
  # message queue, send it out
  $self->process_queue();

  
  return 'a'; # We need to return something here, but we're not using that value. Fixme or check with beanz!

}


=head2 C<xpl_plug(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages.  It handles
the incoming plugwise.basic schema messages.

Supported messages commands are:
 * listcircles : get a list of connected Circles, respond with their ID's

Supported message commands with a device specifier are:
 * on     : switch a circle on
 * off    : switch a circle off
 * status : request the current switch state, internal clock, live power consumption
 
=cut

sub xpl_plug {

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
      if ($command eq 'on') {
        $packet = "0017" . "000D6F0000" . uc($circle) . "01";
      }
      elsif ($command eq 'off') {
        $packet = "0017" . "000D6F0000" . uc($circle) . "00";
      }
      elsif ($command eq 'status') {
        $packet = "0023" . "000D6F0000" . uc($circle);
      }
      elsif ($command eq 'powerinfo') {
        $packet = "0012" . "000D6F0000" . uc($circle);
      }
      elsif ($command eq 'powerbuf') {
        $packet = "0048" . "000D6F0000" . uc($circle) . uc($msg->lastlog);
      }

      # Send the packet to the stick!
      $self->queue_packet_to_stick($packet, "Command") if (defined $packet);

    }
  }

  return 1;
}

=head2 C<stick_init()>

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

sub queue_packet_to_stick {
  my ($self, $packet, $description) = @_;

  my $writeptr = $self->{_write_pointer}++;
  my $readptr  = $self->{_read_pointer};

  $self->{_msg_queue}->{$writeptr}->{packet} = $packet;
  $self->{_msg_queue}->{$writeptr}->{descr} = $description;
 
  $self->print_stats("queue_to_stick");

  $self->process_queue();

  return;
}

sub process_queue {
  my ($self) = @_;

  # If the queue it empty, return
  if ((scalar keys %{$self->{_msg_queue}}) == 0) {
      print "Processing empty queue, returning...\n" if ($self->{_ultraverbose});
      return;
  }

  my $readptr  = $self->{_read_pointer};
  
  # If we're no longer waiting for a response and the response queue is empty, the it is OK to send the next packet
  if (!$self->{_awaiting_stick_response} and (scalar keys %{$self->{_resp_queue}}) == 0) {
      $self->write_packet_to_stick($self->{_msg_queue}->{$readptr}->{packet});

      delete $self->{_msg_queue}->{$readptr};

      $self->{_read_pointer}++;
  }  
  else {
      print "Process_queue: seems we're waiting for a response from a previous command (rd: $readptr)\n" if ($self->{_ultraverbose}); 
  }

  #$self->print_stats("process_queue_end");

}

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

#sub print_uart_buffer {
#    my ($self) = @_;
#    
#    # Print the hex values in the serial RX string
#    my $string = $self->{_uart_rx_buffer};
#    $string =~ s/(.)/sprintf("%02x ",ord($1))/seg;
#    print "UART rx buffer now contains: '$string'\n";    
#
#    return;
#
#}

sub plugwise_crc
{
  sprintf ("%04X", crc($_[0], 16, 0, 0, 0, 0x1021, 0));
}

sub plugwise_process_response
{
  my ($self, $frame) = @_;

  my @xpl_body;
  my $xpl = $self->{_xpl};

  $frame =~ s/(\n|.)*\x05\x05\x03\x03//g; # Strip header
  $frame =~ s/(\r\n)$//; # Strip trailing CRLF

  $xpl->info("RX<STICK: $frame\n");

  # Check if the CRC matches
  if (! (plugwise_crc( substr($frame, 0, -4)) eq substr($frame, -4, 4))) {
      # This is an error to die for...
      $xpl->argh("PLUGWISE: received a frame with an invalid CRC");
      return "";
  }

  # Strip CRC, we already know it is correct
  $frame =~ s/(.{4}$)//;

  # After a command is sent to the stick, we first receive an 'ACK'. This 'ACK' contains a sequence number that we want to track and notifies us of errors.
  if ($frame =~ /^0000([[:xdigit:]]{4})([[:xdigit:]]{4})$/) {
  #      ack          |  seq. nr.     || response code |
    if ($2 eq "00C1") {
      $self->{_response_queue}->{hex($1)}->{received_ok} = 1;
      return "no_xpl_message_required"; # We received ACK from stick, we should not send an xPL message out for this response
    } elsif ($2 eq "00C2"){
      # We sometimes get this reponse on the initial init request, re-init in this case
      $self->write_packet_to_stick("000A");
      return "no_xpl_message_required";
    } else {  
      $xpl->ouch("Received response code with error: $frame\n");
      @xpl_body = [ 'type' => 'err', 'text' => "Received error response", 'message' => $self->{_last_pkt_to_uart}, 'error' => $2 ];
      delete $self->{_response_queue}->{hex($1)};
      $self->{_awaiting_stick_response} = 0;
    
      return \@xpl_body;

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
    @xpl_body = ['command' => 'off', 'device'  => $saddr, 'onoff'   => 'off'];

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is OFF\n");
    return \@xpl_body;      
  }

  #   circle on resp |  seq. nr.     |    | circle MAC
  if ($frame =~/^0000([[:xdigit:]]{4})00D8([[:xdigit:]]{16})$/) {
    my $saddr = $self->addr_l2s($2);
    @xpl_body = ['command' => 'on', 'device'  => $saddr, 'onoff'   => 'on'];

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    $xpl->info("PLUGWISE: Stick reported Circle " . $saddr . " is ON\n");
    return \@xpl_body;      
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
    
    @xpl_body = ['command' => 'powerinfo', 'device'  => $saddr, 'power1'   => $pow1, 'power8' => $pow8];

    return \@xpl_body;
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

    @xpl_body = ('command' => 'listcircles');
    my $count = 0;
    my $device_id;

    foreach $device_id (keys %{$self->{_plugwise}->{circles}}){
	my $device_string = sprintf("device%02i", $count++);
	push @xpl_body, ($device_string => $device_id);
    }

    # Required here, otherwise the message body is not accepted by the framework.
    @xpl_body = [@xpl_body];

    return \@xpl_body;
  }

  # Process the response on a status request
  # status response  |  seq. nr.     ||  Circle+ MAC   || year          || month         || minutes       || curr_log_addr || powerstate
  if ($frame =~/^0024([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{2})([[:xdigit:]]{2})([[:xdigit:]]{4})([[:xdigit:]]{8})([[:xdigit:]]{2})/){
    my $saddr = $self->addr_l2s($2);
    my $onoff = $7 eq '00'? 'off' : 'on';
    $self->{_plugwise}->{circles}->{$saddr}->{onoff} = $onoff;
    $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} = hex($6) - 278528;

    print "Received a status response for $saddr, status=$onoff, logaddr=". $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} . "\n";

    @xpl_body = ['command' => 'status', 'device' => $saddr, 'onoff' => $onoff, 'logaddr' => $self->{_plugwise}->{circles}->{$saddr}->{curr_logaddr} ];

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    return \@xpl_body;
  }

  # Process the response on a calibration request
  if ($frame =~/^0027([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{8})([[:xdigit:]]{8})$/){
  # calibration resp |  seq. nr.     ||  Circle+ MAC   || gainA         || gainB         || offtot        || offruis
    print "Received for $2 calibration response!\n";
    my $saddr = $self->addr_l2s($2);
    print "Short address  = $saddr\n";
    $self->{_plugwise}->{circles}->{$saddr}->{gainA}   = $self->hex2float($3);
    $self->{_plugwise}->{circles}->{$saddr}->{gainB}   = $self->hex2float($4);
    $self->{_plugwise}->{circles}->{$saddr}->{offtot}  = $self->hex2float($5);
    $self->{_plugwise}->{circles}->{$saddr}->{offruis} = $self->hex2float($6);

    # Update the response_queue, remove the entry corresponding to this reply 
    delete $self->{_response_queue}->{hex($1)};

    return "no_xpl_message_required";      
  }

  # Temporary workaround while we're implementing the rest of the protocol
  #$self->{_resp_pointer}++;
  $xpl->ouch("Received unknown response: '$frame'");
  return "no_xpl_message_required";

  # 28 = ON/OFF, 56 = Calibration info, 66 = Status info, 40 = powerinfo, 96 = powerbuf
  #if ( (length($t) == 28) || (length($t) == 56) || (length($t) == 66) || (length($t) == 40) || (length($t) == 96))
  #{
  #    print "Got a valid response"
  #} else {
  #    print "Received invalid response $t\n";
  #    return "";
  #}
}

sub hex2int   { unpack("N", pack("H8", substr('0'x8 .$_[0], -8))) }

sub hex2float {
    my ($self, $hexstr) = @_;

    my $floater = unpack('f', reverse pack('H*', $hexstr));

    return $floater;
}

sub pulsecorrection {
    my ($self, $id) = @_;

    my $value1 = hex($self->{_plugwise}->{circles}->{$id}->{pulse1});
    my $value8 = hex($self->{_plugwise}->{circles}->{$id}->{pulse8})/8;

    my $offnoise = $self->{_plugwise}->{circles}->{$id}->{offruis};
    my $offtot   = $self->{_plugwise}->{circles}->{$id}->{offtot};
    my $gainA    = $self->{_plugwise}->{circles}->{$id}->{gainA};
    my $gainB    = $self->{_plugwise}->{circles}->{$id}->{gainB};

    my $out1 = (($value1 + $offnoise) ^ 2) * $gainB + (($value1 + $offnoise ) * $gainA ) + $offtot;
    my $out8 = (($value8 + $offnoise) ^ 2) * $gainB + (($value8 + $offnoise ) * $gainA ) + $offtot;

    print "$offnoise - $offtot - $gainA - $gainB\n";
    print "Pulses 1: $value1 - corrected: $out1\n";
    print "Pulses 8: $value8 - corrected: $out8\n";

    return ($out1, $out8);

}
sub calc_live_power {
    my ($self, $id) =@_;

    my ($pulse1, $pulse8) = $self->pulsecorrection($id);

    my ($live1, $live8);

    $live1 = $pulse1 * 1000 / 468.9385193;
    $live8 = $pulse8 * 1000 / 468.9385193;

    return ($live1, $live8);

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
	my $packet   = "0018" . "000D6F0000" . $self->{_plugwise}->{coordinator_MAC} . $strindex;
	$self->queue_packet_to_stick($packet, "Query connected device $strindex");
    }

    return;
}

# Convert the long Circle address notation to short
sub addr_l2s {
    my ($self,$address) = @_;
    return substr($address, -6, 6);
}

# Convert the short Circle address notation to long
sub addr_s2l {
    my ($self,$address) = @_;
    return "000D6F0000" . $address;
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
