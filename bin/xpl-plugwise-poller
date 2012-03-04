#!/usr/bin/perl -w

=head1 NAME

xpl-plugwise-poller - Request the Plugwise measured power consumption

=head1 SYNOPSIS

  xpl-plugwise-poller [flags] [options] <device_id>
  where valid flags are:
    -h - show this help text
    -v - verbose client mode
  and valid options are (default shown in brackets):
    -i if0 - the interface for xPL messages (first non-loopback or loopback)

  # start the rrd listening and broadcasting on first Ethernet
  # interface in verbose mode
  xpl-plugwise-poller -i eth0 -v hollie-plugwise.ground_floor

=head1 DESCRIPTION

This script will poll the circles connected to <device_id> for their status.
When it notices a new historical power value is available, it will create a 
request for power information.

=cut

use 5.006;
use strict;
use warnings;
use Getopt::Long;
use Pod::Usage;
use xPL::Client;
use Data::Dumper;

$|=1; # autoflush helps debugging

my %args = ( vendor_id => 'hollie', device_id => 'plgwpoll' );
my %opt = ();
my $verbose;
my $interface;
my $help;
my $man;
GetOptions('verbose+' => \$verbose,
           'interface=s' => \$interface,
           'define=s' => \%opt,
           'help|?|h' => \$help,
           'man' => \$man
          ) or pod2usage(2);
pod2usage(1) if ($help);
pod2usage(-exitstatus => 0, -verbose => 2) if ($man);

$args{'interface'} = $interface if ($interface);
$args{'verbose'} = $verbose if ($verbose);

my $target = shift or pod2usage(-message =>
                        'The target xPL device id is required',
                      -exitstatus => 1);

my $data;

# Create an xPL Client object
my $xpl = xPL::Client->new(%args, %opt) or die "Failed to create xPL::Client\n";

# Add a callback to receive all incoming xPL messages
$xpl->add_xpl_callback(id => 'xpl',
                       self_skip => 1, targeted => 0,
                       callback => \&xpl_msg);
$xpl->add_timer(id => 'status_request', timeout => 10, callback => \&request_status);

# Run the main loop
$xpl->main_loop();

# The callback to monitor the incoming messages
sub xpl_msg {
  my %p = @_;
  my $msg = $p{message};
  my $time = time;
  my $c = chr(0);

  my $schema = $msg->schema;
  my $device = $msg->field('device');
  my $type = $msg->field('type');
  my $current = $msg->field('current');
  my $command = $msg->field('command');
  
  # Process response of listcircles command
  if ($msg->schema eq 'plugwise.basic' && $command eq 'listcircles') {
    print "Received listcircles response: \n";
    foreach ($msg->body_fields()){
      if (/(device.+)/) {
        print "Device $1 found: ";
        print $msg->field($1) . "\n";
      }
    }
    
  } 
  return 1;
}

sub request_status {
  my %p = @_;
  
  
  my %xplmsg = (
      message_type => 'xpl-cmnd',
      schema => 'plugwise.basic',
      head => { 'target' => $target },
      );
  
  if (!defined ($data->{got_list})){
    # First we need to pull the list of known devices from the network coordinator
    $xplmsg{body} = [ 'command' => 'listcircles' ];
  }
    
  $xpl->send(%xplmsg);
}


# send a "hbeat.end" message on exit
END { defined $xpl && $xpl->send_hbeat_end(); }

=head1 SEE ALSO

xPL::Client(3), xPL::Listener(3), rrdtool(1), RRDs(3)

Project website: http://www.xpl-perl.org.uk/

ZenAH Project website: http://www.zenah.org.uk/

=head1 AUTHOR

Mark Hindess, E<lt>soft-xpl-perl@temporalanomaly.comE<gt>

=head1 COPYRIGHT

Copyright (C) 2006, 2010 by Mark Hindess

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
