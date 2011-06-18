package xPL::Dock::Plugwise;

=head1 NAME

xPL::Dock::Plugwise - xPL::Dock plugin for a Plugwise USB stick

=head1 SYNOPSIS

  use xPL::Dock qw/Plugwise/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds control of a Plugwise network through a Stick

Current implemented functions:

Switching ON/OFF of circles

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
     'plugwise-verbose+' => \$self->{_verbose},
     'plugwise-tty=s' => \$self->{_device},
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

  # Init the buffer that will be used for serial data reception
  $self->{_uart_rx_buffer} = "";

  # Init the read and write pointer in the plugwise message queue
  $self->{_read_pointer}  = 0;
  $self->{_write_pointer} = 0;
  $self->{_resp_pointer}  = 0;

  # Send the init message to connect to the stick
  #$self->stick_init($io);

  return $self;
}

=head2 C<vendor_id()>

Defines the vendor ID for the PlugWise plugin. Doesn't seem to propagate?

=cut

sub vendor_id {
  'bnz'
}

=head2 C<device_reader()>

This is the callback that processes output from the Plugwise USB stick.  It is
responsible for sending out the xPL messages.

Current implementation involves sending out confirmation messages about the
status of Circles that are switched on/off

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  print "New message part received from UART: '$new_msg'\n";

  my $xpl = $self->xpl;

  # Append newly received string to existing buffer
  $self->{_uart_rx_buffer} .= $new_msg;

  my $frame;


  # As long we have response packets in the received buffer, process them
  while ($self->{_uart_rx_buffer} =~ /(\x05\x05\x03\x03\w+\r\n)(.*)/s) { # < we need 's' modifier here because we also want to match '\n' with the '.'

      $frame = $1;
      $self->{_uart_rx_buffer} = $2; # Cut the part we're going to process from the buffer

      print "Found frame $frame - remaining: $self->{_uart_rx_buffer}\n";

      # Processes the received frame, this is done in another routine to split the Plugwise protocol implementation and the xPL packet generation
      my $result=$self->plugwise_process_response($frame); 

      $self->print_stats("device_reader");

      next if ($result eq "no_xpl_message_required"); # No packet received that needs an xPL message to be sent

      my %xplmsg = (
	  message_type => 'xpl-trig',
          schema => 'plugwise.basic',
          body => @{$result},
      );

      print Dumper(%xplmsg);

      $xpl->send(%xplmsg);


      my $function_code = 0x66; #hex2int (substr($result, 0, 4)); # Function code

      if ($function_code == 0x0) { # Message is reponse to switching on/off a circle
	  $xplmsg{'body'}{'command'} = ((hex2int(substr($result, 4, 4)) & 0x2) >> 1) ? 'off' : 'on';
	  $xplmsg{'body'}{'device'}  = substr($result, 18, 6); # Macaddress
	  $xplmsg{'body'}{'onoff'} = ((hex2int(substr($result, 4, 4)) & 0x2) >> 1) ? 'off' : 'on';
      }

      elsif ($function_code == 0x27) { # Message is reponse to calibrationinfo request
	  $xplmsg{'body'}{'command'} = 'calibrate';
	  $xplmsg{'body'}{'device'}  = substr($result, 14, 6); # Macaddress
	  $xplmsg{'body'}{'gaina'}   = substr($result, 20, 8); # GainA
	  $xplmsg{'body'}{'gainb'}   = substr($result, 28, 8); # GainB
	  $xplmsg{'body'}{'offtot'}  = substr($result, 36, 8); # OffTot
	  $xplmsg{'body'}{'offruis'} = substr($result, 44, 8); # OffRuis
      }

      elsif ($function_code == 0x24) { # Message is response to status info request
	  $xplmsg{'body'}{'command'} = 'status';
	  $xplmsg{'body'}{'device'}  = substr($result, 14, 6); # Macaddress
	  $xplmsg{'body'}{'abshour'} = substr($result, 20, 8); # Hours passed since 1-6-2007 00:00
	  $xplmsg{'body'}{'lastlog'} = substr($result, 28, 8); # Last log address available
	  $xplmsg{'body'}{'onoff'}   = hex2int(substr($result, 36,2)) ? 'on' : 'off';
	  $xplmsg{'body'}{'hwver'}   = substr($result, 40, 12); # HW version of the circle
      }

      elsif ($function_code == 0x13) { # Message is response to power info request 
	  $xplmsg{'body'}{'command'}   = 'powerinfo';
	  $xplmsg{'body'}{'device'}    = substr($result, 14, 6); # Macaddress
	  $xplmsg{'body'}{'pulse8sec'} = substr($result, 20, 4); # Pulse information of 8 seconds reading
	  $xplmsg{'body'}{'pulse1sec'} = substr($result, 24, 4); # Pulse information of 1 second reading
	  $xplmsg{'body'}{'unknown'}   = substr($result, 28, 8); # Yet unknown what this means
      }

      elsif ($function_code == 0x49) { # Message is response to power buffer request
	  $xplmsg{'body'}{'command'}     = 'powerbuf';
	  $xplmsg{'body'}{'device'}      = substr($result, 14, 6); # Macaddress
	  $xplmsg{'body'}{'firstbuf'}    = substr($result, 20, 8); # Hour of 1st buffer (abshour format)
	  $xplmsg{'body'}{'firstpulse'}  = substr($result, 28, 8); # Usage in pulses 1st hour
	  $xplmsg{'body'}{'secondbuf'}   = substr($result, 36, 8); # Hour of 2nd buffer (abshour format)
	  $xplmsg{'body'}{'secondpulse'} = substr($result, 44, 8); # Usage in pulses 2nd hour
	  $xplmsg{'body'}{'thirdbuf'}    = substr($result, 52, 8); # Hour of 3rd buffer (abshour format)
	  $xplmsg{'body'}{'thirdpulse'}  = substr($result, 60, 8); # Usage in pulses 3rd hour
	  $xplmsg{'body'}{'fourthbuf'}   = substr($result, 68, 8); # Hour of 4th buffer (abshour format)
	  $xplmsg{'body'}{'fourthpulse'} = substr($result, 76, 8); # Usage in pulses 4th hour
	  $xplmsg{'body'}{'curlogaddr'}  = substr($result, 84, 8); # Log address of current buffer
      }

  }

  $self->print_stats("end og device reader");

  # Check if we need to send another message to the stick, if there is a message left in the 
  # message queue, send it out
  $self->process_queue();
  print "Falling out of device reader...\n";

}


=head2 C<xpl_plug(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages.  It handles
the incoming plugwise.basic schema messages.

=cut

sub xpl_plug {

  my %p = @_;
  my $msg = $p{message};
  my $self = $p{arguments};

  my $packet;

  print Dumper($msg);


  if (! $self->{_plugwise}->{connected}){
    print "Not yet connect to stick, init first\n";
    $self->stick_init();
  }

  if ($msg->field('device')) {
    my $command = lc($msg->field('command'));
    foreach my $circle (split /,/, $msg->field('device')) {
      if ($command eq 'on') {
        $packet = "0017" . "000D6F0000" . uc($circle) . "01";
      }
      elsif ($command eq 'off') {
        $packet = "0017" . "000D6F0000" . uc($circle) . "00";
      }
      elsif ($command eq 'calibrate') {
        $packet = "0026" . "000D6F0000" . uc($circle);
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
      elsif ($command eq 'debug') {

	  return 1;
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
  $self->queue_packet_to_stick("000A", "Init request");

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

  print "WR>STICK: $packet\n";

  $packet = "\05\05\03\03" . $packet; # Add header (crlf termination is handled by io handle)

  $self->{_io}->write($packet);

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
      print "Processing empty queue, returning...\n";
      return;
  }

  my $resptr = $self->{_resp_pointer};
  my $readptr  = $self->{_read_pointer};
  
  if ($resptr == $readptr) {
      $self->write_packet_to_stick($self->{_msg_queue}->{$readptr}->{packet});

      delete $self->{_msg_queue}->{$readptr};

      $self->{_read_pointer}++;

  } else {
      print "Process_queue: seems we're waiting for a response from a previous command\n";
  }

  $self->print_stats("process_queue_end");

}

sub print_stats {
    my ($self, $fname) = @_;
    print "------- $fname -------------------------------\n";
    print "Message queue is:\n";
    print Dumper($self->{_msg_queue});
    print "Response queue is: \n";
    print Dumper($self->{_response_queue});
    print "Plugwise connected status is $self->{_plugwise}->{connected}\n";
    print "UART RX buffer is now '$self->{_uart_rx_buffer}'\n";
    print "Pointers: RD: $self->{_read_pointer} - RESP: $self->{_resp_pointer}\n";
    print "++++++++++++++++++++++++++++++++++++++\n";

}

sub print_uart_buffer {
    my ($self) = @_;
    
    # Print the hex values in the serial RX string
    my $string = $self->{_uart_rx_buffer};
    $string =~ s/(.)/sprintf("%02x ",ord($1))/seg;
    print "UART rx buffer now contains: '$string'\n";    

    return;

}
sub plugwise_crc
{
  sprintf ("%04X", crc($_[0], 16, 0, 0, 0, 0x1021, 0));
}

sub plugwise_process_response
{
  my ($self, $frame) = @_;

  my @xpl_body;

  $frame =~ s/(\n|.)*\x05\x05\x03\x03//g; # Strip header
  $frame =~ s/(\r\n)$//; # Strip trailing CRLF

  # Check if the CRC matches
  if (! (plugwise_crc( substr($frame, 0, -4)) eq substr($frame, -4, 4))) {
      print "Received invalid CRC in frame $frame\n";
      # TODO handle this error state
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
      print "Received response code with error: $frame\n";
      $self->{_response_queue}->{hex($1)}->{received_ok} = 0;
      $self->{_response_queue}->{hex($1)}->{frame} = $frame;
      return "no_xpl_message_required";
    }
  }

  if ($frame =~ /^0011([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})([[:xdigit:]]{16})([[:xdigit:]]{4})/) {
  #     init response |  seq. nr.     || stick MAC addr || don't care    || network key    || short key
    $self->{_plugwise}->{stick_MAC}   = $2;
    $self->{_plugwise}->{network_key} = $4;
    $self->{_plugwise}->{short_key}   = $5;
    $self->{_plugwise}->{connected}   = 1;
    # Delete entry in command queue
    delete $self->{_response_queue}->{hex($1)};
    # Increment response counter
    $self->{_resp_pointer}++;
    return "no_xpl_message_required";
  }

  if ($frame =~/^0000([[:xdigit:]]{4})00DE([[:xdigit:]]{16})$/) {
  #     cmnd off resp|  seq. nr.     |    | plug MAC
    @xpl_body = ['command' => 'off', 'device'  => substr($2, -6, 6), 'onoff'   => 'off'];
    # Delete entry in command queue
    delete $self->{_response_queue}->{hex($1)};
    # Increment response counter
    $self->{_resp_pointer}++;
    return \@xpl_body;      
  }

  if ($frame =~/^0000([[:xdigit:]]{4})00D8([[:xdigit:]]{16})$/) {
  #     cmnd on resp |  seq. nr.     |    | plug MAC
    @xpl_body = ['command' => 'on', 'device'  => substr($2, -6, 6), 'onoff'   => 'on'];

    # Delete entry in command queue
    delete $self->{_response_queue}->{hex($1)};
    # Increment response counter
    $self->{_resp_pointer}++;
    return \@xpl_body;      
  }

  # Temporary workaround while we're implementing the rest of the protocol
  $self->{_resp_pointer}++;
  print "Received unknown response: '$frame'\n";
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

sub query_connected_circles {

    my $self = @_;

    # In this code we will scan all connected circles to be able to add them to the $self->{_plugwise}->{circles} hash
    my $index =0;

    while ($index < 3) {
	queue_packet_to_stick("");
    }
}

1;
__END__

=head1 EXPORT

None by default.

=head1 SEE ALSO

Project website: http://www.xpl-perl.org.uk/

=head1 AUTHOR

Jfn, E<lt>pe1pqf@REMOVE_THISzonnet.nlE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2009 by Mark Hindess / Jfn

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
