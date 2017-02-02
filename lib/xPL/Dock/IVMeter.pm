package xPL::Dock::IVMeter;

=head1 NAME

xPL::Dock::IVMeter - xPL::Dock plugin for a current/voltage meter

=head1 SYNOPSIS

  use xPL::Dock qw/IVMeter/;
  my $xpl = xPL::Dock->new();
  $xpl->main_loop();

=head1 DESCRIPTION

This L<xPL::Dock> plugin adds reception of a current/voltage meter that reports
the readings over a serial port.

The device will report when the readings change and will by default send a packet every minute
when the reading is static in order to keep the RRD graphs up to date.

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
  $self->{_baud} = 115200;
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
                        input_record_type => 'xPL::IORecord::CRLFLine',
                        ack_timeout_callback => 3,
                        output_record_type => 'xPL::IORecord::CRLFLine' );

  # Add a timer for regular interval reporting
  $xpl->add_timer(id => 'ivmeter',
                  timeout => 60,
                  callback => sub { $self->reset_reading(); 1; });

  # Init the device name based on the serial of the USB dongle
  if ($self->{_device} =~ /.+\-(\w+)$/) {
    $self->{_device_name} = $1;
  } elsif ($self->{_device} =~ /\/dev\/ttyUSB/) {
    print "We're on a device that has non-unique serial port names, trying to fetch the unique number\n";
    my $cmd = "udevadm info -a -n $self->{_device} | grep '{serial}' | head -n1";
    my $unique_id = `$cmd`;
    #print "Response from command: " . $unique_id;

    if ($unique_id =~ /=="(\w+)"/) {
	$self->{_device_name} = $1;
    } else {
	$self->{_device_name} = 'non-unique';
    }
 
  } else {
    $self->{_device_name} = $self->{_device};
  }

  print "Device name is set to: " . $self->{_device_name} . "\n";

  $self->reset_reading();

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

sub reset_reading {
    my $self = shift();

    $self->{_last}->{current} = -100;
    $self->{_last}->{voltage} = -100;

}
=head2 C<device_reader( )>

This is the callback that processes output from the meter.  It is
responsible for sending out the xPL messages.

=cut

sub device_reader {
  my ($self, $handler, $new_msg, $last) = @_;

  #print "New message received from UART: '$new_msg'\n";

  my $xpl = $self->{_xpl};

  # Parse it
  if ($new_msg =~ /^I=\s+(\d+)\s+;\s+V=\s+(\d+)$/){
    my $voltage = $2 / 100;
    my $current = $1;

    my $last = $self->{_last};

    if ($voltage != $last->{voltage} || abs($current - $last->{current}) > 2) {
      $last->{voltage} = $voltage;
      $last->{current} = $current;
      $self->report();
    }

    #print "Current: $current, voltage: $voltage\n";



  } else {
    $xpl->info("Received invalid message from stick: $new_msg\n");
  }


  return;

}

sub report {
  my $self = shift();

  my $last = $self->{_last};
  my $xpl = $self->{_xpl};

  my %xplmsg = (
      message_type => 'xpl-trig',
      schema => 'sensor.basic',
      );

  $xplmsg{body} = ['device'  => $self->{_device_name}, 'type' => 'current', 'current' => $last->{current}, 'units' => 'uA'];

  print "Sending xpl message with payload $last->{current}\n";
  $xpl->send(%xplmsg);

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
