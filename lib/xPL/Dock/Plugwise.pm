package xPL::Dock::Plugwise;

=head1 NAME

xPL::Dock::Plugwise - xPL::Dock plugin for an Plugwise USB stick

=head1 SYNOPSIS

  use xPL::Dock qw/Plugwise/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This module creates an xPL client for a serial port-based device.  There
are several usage examples provided by the xPL Perl distribution.

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
     'plugwise-baud=i' => \$self->{_baud},
     'plugwise-tty=s' => \$self->{_device},
    );
}

=head2 C<init(%params)>

=cut

sub plugwise_crc
{
  sprintf ("%04X", crc($_[0], 16, 0, 0, 0, 0x1021, 0));
}

sub plugwise_valid_response
{
  my $t=$_[0];
  $t =~ s/(\n|.)*\x05\x05\x03\x03//g; # Strip header
  $t =~ s/(\r\n)$//; # Strip trailing CRLF

  # 28 = ON/OFF, 56 = Calibration info, 66 = Status info, 40 = powerinfo, 96 = powerbuf
  if ( (length($t) == 28) || (length($t) == 56) || (length($t) == 66) || (length($t) == 40) || (length($t) == 96))
  {
    return $t if ( plugwise_crc( substr($t, 0, -4)) eq substr($t, -4, 4)); # Valid checksum, so return $t
  }
}

sub hex2int   { unpack("N", pack("H8", substr('0'x8 .$_[0], -8))) }

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

  return $self;
}

=head2 C<device_reader()>

This is the callback that processes output from the Plugwise USB stick.  It is
responsible for sending out the xPL messages.

Current implementation involves sending out confirmation messages about the
status of Circles that are switched on/off

=cut

sub device_reader {
  my ($self, $handler, $msg, $last) = @_;

  my $xpl = $self->xpl;

  my $result=plugwise_valid_response($msg); # Validate received packet

  return 1 if ($result eq ""); # No valid packet received, just return
  print "Packet received: $result\n";

  my %xplmsg = (
                 message_type => 'xpl-trig',
                 class => 'plugwise.basic',
                 body => {},
               );

  my $function_code = hex2int (substr($result, 0, 4)); # Function code

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
    
  $xpl->send(%xplmsg);

  return 1;
}


=head2 C<xpl_plug(%xpl_callback_parameters)>

This is the callback that processes incoming xPL messages.  It handles
the incoming plugwise.basic schema messages.

=cut

sub xpl_plug {
  my %p = @_;
  my $msg = $p{message};
  my $peeraddr = $p{peeraddr};
  my $peerport = $p{peerport};
  my $self = $p{arguments};
  my $packet;
 
  if ($msg->device) {
    my $command = lc($msg->command);
    foreach my $circle (split /,/, $msg->device) {
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

      if (defined $packet) {
        $packet .= plugwise_crc ($packet); # Add CRC
        $packet = "\05\05\03\03" . $packet; # Add header (crlf termination is handled by io handle)

        $self->{_io}->write($packet);
      }

    }
  }

  return 1;
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
