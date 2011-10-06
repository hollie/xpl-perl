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
  # $xpl->add_xpl_callback(id => 'control-basic', callback => \&xpl_control,
  #     arguments => $self,
  #     filter =>
  #     {
  #      message_type => 'xpl-cmnd',
  #      class => 'control',
  #      class_type => 'basic',
  #     });

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

  # print "UART buffer now looks like: '" . $self->{_uart_rx_buffer} . "'\n";

  my $frame;

  # As long we have response packets in the received buffer, process them
  while ($self->{_uart_rx_buffer} =~ /(.+\n)(.*)/s) { # < we need 's' modifier here because we also want to match '\n' with the '.'

      $frame = $1;
      $self->{_uart_rx_buffer} = $2; # Cut the part we're going to process from the buffer

      #print "Found frame $frame - remaining: $self->{_uart_rx_buffer}\n" if ($self->{_ultraverbose});

      # Processes the received frame, this is done in another routine to keep the protocol section separated from the hardware interfacing code
      # This function will return either a message that tells not to send and xPL message, or an actual xPL message to be sent
      my $result=$self->plugwise_process_response($frame); 

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
     #  $circle = uc($circle);
 
     # if ($current eq 'enable') {
     #    $packet = "0017" . "000D6F0000" . $circle . "01";
     #  }

     #  elsif ($current eq 'disable') {
     #    $packet = "0017" . "000D6F0000" . $circle . "00";
     #  }

     #  else {
     #    $xpl->info("internal: Received invalid command '$current'\n");
     #  }

     #  # Send the packet to the stick!
     #  $self->queue_packet_to_stick($packet, "control.basic") if (defined $packet);

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
  $self->write_packet_to_stick("z");

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

    $xpl->send(%xplmsg);

    $xplmsg{body} = ['device'  => "room".$id, 'type' => 'temp', 'current' => $temp/10];

    $xpl->send(%xplmsg);

    $xplmsg{body} = ['device'  => "room".$id, 'type' => 'humidity', 'current' => $humi];

    $xpl->send(%xplmsg);

    $xplmsg{body} = ['device'  => "room".$id, 'type' => 'light', 'current' => int($light/255*100)];

    $xpl->send(%xplmsg);


    return;

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

This code is under development.

=head1 AUTHOR

Lieven Hollevoet, E<lt>lieven@lika.beE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2011 by Mark Hindess / Lieven Hollevoet

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
