package xPL::Client;

# $Id$

=head1 NAME

xPL::Client - Perl extension for an xPL Client

=head1 SYNOPSIS

  use xPL::Client;

  my $xpl = xPL::Client->new(id => 'acme-clock.default');
  $xpl->add_timer(name => 'tick',
                  timeout => 1,
                  callback => sub { $xpl->tick(@_) },
                  );

  $xpl->main_loop();

=head1 DESCRIPTION

This module creates an xPL client.

=head1 METHODS

=cut

use 5.006;
use strict;
use warnings;

my $HYPHEN = q{-};
my $DOT = q{.};

use POSIX qw/uname/;
use Socket;
use Time::HiRes;
use xPL::Listener;

use Exporter;
#use AutoLoader qw(AUTOLOAD);

our @ISA = qw(xPL::Listener);
our %EXPORT_TAGS = ( 'all' => [ qw() ] );
our @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
our @EXPORT = qw();
our $VERSION = qw/$Revision$/[1];

my @attributes =
  qw/hbeat_mode vendor_id device_id
     hbeat_interval fast_hbeat_interval hopeful_hbeat_interval
     hub_response_timeout hbeat_count/;
foreach my $a (@attributes) {
  __PACKAGE__->make_readonly_accessor($a);
}

=head2 C<new(%params)>

The constructor creates a new xPL::Client object.  The constructor
takes a parameter hash as arguments.  Valid parameters in the hash
are:

=over 4

=item id

  The identity for this client.

=item interface

  The interface to use.  The default is to use the first active
  interface that isn't the loopback interface if there is one, or the
  loopback interface if that is the only one.  (Not yet implemented.)

=item ip

  The IP address to bind to.  This can be used instead of the
  'interface' parameter and will take precedent over the 'interface'
  parameter if they are in conflict.

=item broadcast

  The broadcast address to use.  This is required if the 'ip'
  parameter has been given.

=back

It returns a blessed reference when successful or undef otherwise.

=cut

sub new {
  my $pkg = shift;
  if (ref $pkg) { $pkg = ref $pkg }

  my %p = @_;
  my $self = $pkg->SUPER::new(@_);
  $self->{_hbeat_count} = 0;

  foreach (qw/vendor_id device_id/) {
    exists $p{$_} or $self->argh("requires '$_' parameter");
    $p{$_} =~ /^[A-Za-z0-9]{1,8}$/ or $self->argh("$_ invalid");
  }

  exists $p{instance_id} or
    $p{instance_id} = substr $ENV{XPL_HOSTNAME}||(uname)[1]||'default', 0, 16;
  $p{instance_id} =~ s/\..*$//; # strip domain if there is one
  $p{instance_id}=~/^[A-Za-z0-9]{1,16}$/ or
    $self->argh('instance_id, '.$p{instance_id}.", is invalid.\n".
      "The default can be overriden by setting the XPL_HOSTNAME environment\n".
      'variable');

  exists $p{hbeat_interval} or $p{hbeat_interval} = 5;
  ($p{hbeat_interval} =~ /^[\d]+$/ &&
   $p{hbeat_interval} >= 5 && $p{hbeat_interval} <= 30) or
     $self->argh('hbeat_interval is invalid: should be 5 - 30 (minutes)');

  exists $p{fast_hbeat_interval} or $p{fast_hbeat_interval} = 3;
  ($p{fast_hbeat_interval} =~ /^[\d]+$/ &&
   $p{fast_hbeat_interval} >= 3 && $p{fast_hbeat_interval} <= 30) or
     $self->argh('fast_hbeat_interval is invalid: '.
                 'should be 3 - 30 (seconds)');

  exists $p{hopeful_hbeat_interval} or $p{hopeful_hbeat_interval} = 30;
  ($p{hopeful_hbeat_interval} =~ /^[\d]+$/ &&
   $p{hopeful_hbeat_interval} >= 20 &&
   $p{hopeful_hbeat_interval} <= 300) or
     $self->argh('hopeful_hbeat_interval is invalid: '.
                 'should be 20 - 300 (seconds)');

  exists $p{hub_response_timeout} or $p{hub_response_timeout} = 120;
  ($p{hub_response_timeout} =~ /^[\d]+$/ &&
   $p{hub_response_timeout} >= 30 && $p{hub_response_timeout} <= 300) or
     $self->argh('hub_response_timeout is invalid: '.
                 'should be 30 - 300 (seconds)');

  foreach (qw/vendor_id device_id instance_id
              hbeat_interval fast_hbeat_interval
              hopeful_hbeat_interval hub_response_timeout/) {
    $self->{'_'.$_} = $p{$_};
  }

  $self->{_max_fast_hbeat_count} =
    int $self->hub_response_timeout / $self->fast_hbeat_interval;

  $self->fast_hbeat_mode();

  $self->add_xpl_callback(id => '!hbeat-request',
                          self_skip => 0,
                          filter =>
                          {
                           message_type => 'xpl-cmnd',
                           class => 'hbeat',
                           class_type => 'request',
                          },
                          callback => sub { $self->hbeat_request(@_) });

  $self->add_xpl_callback(id => '!ping-request',
                          self_skip => 0,
                          filter =>
                          {
                           message_type => 'xpl-cmnd',
                           class => 'ping',
                           class_type => 'request',
                          },
                          callback => sub { $self->ping_request(@_) });

  return $self;
}

=head2 C<hbeat_mode()>

Returns the current hbeat mode for this client.  Possible values
are:

=over

=item fast

  During the initial interval when hbeats are sent quickly.

=item hopeful

  When the hub has failed to respond to the initial fast hbeats
  and hbeats are being sent at a moderate rate in the hope that
  the hub might appear.

=item standard

  When the hub has responded and hbeats are being sent at the
  standard interval.

=back

=head2 C<id()>

Returns the identity for this source.

=cut

sub id {
  my $self = shift;
  $self->ouch('called with an argument, but id is readonly') if (@_);
  return
    $self->{_vendor_id}.$HYPHEN.$self->{_device_id}.$DOT.$self->{_instance_id};
}

=head2 C<vendor_id()>

Returns the vendor ID for this source.

=head2 C<device_id()>

Returns the device ID for this source.

=head2 C<instance_id()>

Returns the instance ID for this source.

=cut

sub instance_id {
  my $self = shift;
  if (@_) {
    my $id = $_[0];
    $self->ouch("invalid instance_id '$id'")
      unless ($id =~ qr/^[A-Za-z0-9]{1,12}$/);
    $self->{_instance_id} = substr $_[0], 0, 12;
  }
  return $self->{_instance_id};
}

=head2 C<hbeat_interval()>

Returns the hbeat interval (in minutes) for this source.

=head2 C<fast_hbeat_interval()>

Returns the fast/initial hbeat interval (in seconds) for this source.

=head2 C<hopeful_hbeat_interval()>

Returns the hopeful hbeat interval (in seconds) for this source.
This is used if the hub fails to respond to the initial fast hbeat
messages.

=head2 C<hub_response_timeout()>

Returns the hub response timeout (in seconds) for this source.  This
is the amount of time that fast hbeat messages are sent before the
client backs off and sends hbeat messages at the slower hopeful hbeat
interval.

=head2 C<fast_hbeat_mode()>

This method puts the client into the fast hbeat mode - typically when
the client is initially started up.

=cut

sub fast_hbeat_mode {
  my $self = shift;

  $self->{_hbeat_mode} = 'fast';

  $self->add_timer(id => '!fast-hbeat',
                   # negative so it's triggered ASAP
                   timeout => -$self->fast_hbeat_interval(),
                   callback => sub { $self->fast_hbeat(); },
                  );

  $self->add_xpl_callback(id => '!hub-found',
                          self_skip => 0,
                          filter =>
                          {
                           class => 'hbeat',
                           class_type => 'app',
                           source => $self->id,
                          },
                          callback => sub { $self->hub_response(@_) });
  return 1;
}

=head2 C<fast_hbeat_mode()>

This method puts the client into the standard hbeat mode - typically after
the client has received a response from the hub.

=cut

sub standard_hbeat_mode {
  my $self = shift;

  $self->{_hbeat_mode} = 'standard';
  $self->add_timer(id => '!hbeat',
                   timeout => $self->hbeat_interval*60,
                   callback => sub { $self->send_hbeat(@_) },
                  );
  return 1;
}

=head2 C<fast_hbeat()>

This method is the callback that sends the hbeats when the
client is in fast and hopeful mode.  It is also responsible
for reducing the frequency of the hbeat messages if the
hub fails to respond.

=cut

sub fast_hbeat {
  my $self = shift;
  $self->send_hbeat();
  if ($self->{_hbeat_count} == $self->{_max_fast_hbeat_count}) {
    $self->{_hbeat_mode} = 'hopeful';
    $self->remove_timer('!fast-hbeat');
    $self->add_timer(id => '!fast-hbeat',
                     timeout => $self->hopeful_hbeat_interval,
                     callback => sub {
                       $self->send_hbeat();
                     });
  }
  return 1;
}

=head2 C<hub_response()>

This method is the callback is used to check for a response from the
local hub.  If it sees the hub response, it removes itself and the
fast (or hopeful) hbeat timer from the main loop and adds the
standard rate hbeat timer.

=cut

sub hub_response {
  my $self = shift;
  my %p = @_;
  my $msg = $p{message};


  # we have a winner, our hbeat has been returned
  $self->remove_timer('!fast-hbeat');
  $self->remove_xpl_callback('!hub-found');

  $self->standard_hbeat_mode();

  return 1;
}

=head2 C<hbeat_request()>

This method is the callback is used to handle C<hbeat.request> messages.

=cut

sub hbeat_request {
  my $self = shift;
  my %p = @_;
  my $msg = $p{message};

  $self->add_timer(id => '!hbeat-response',
                   timeout => 2 + rand 4,
                   callback => sub { $self->send_extra_hbeat(@_); return 0; },
                  );
  return 1;
}

=head2 C<ping_request()>

This method is the callback is used to handle C<ping.request> messages.

=cut

sub ping_request {
  my $self = shift;
  my %p = @_;
  my $msg = $p{message};

  if ($self->exists_timer('!ping-response')) {
    # we are about to respond anyway so do nothing
    return 1;
  }
  $self->ping_check();
  my $delay = 2 + rand 4;
  $self->add_timer(id => '!ping-response',
                   timeout => $delay,
                   callback =>
                     sub { $self->send_ping_response($delay, @_); return 0; },
                  );
  return 1;
}

=head2 C<ping_action()>

This method is intended to confirm that the client is functioning
correctly.  The default implementation simply calls L<ping_done> with
the string argument, 'ok'.  It is intended to be overriden by clients
to provide more substancial functionality to confirm (or not) that the
client is really functioning correctly.  It is intended that the
checking is asynchonous so strictly-speaking this method should begin
the checking process.

=cut

sub ping_action {
  my $self = shift;
  $self->ping_done('ok');
}

=head2 C<ping_kill_action()>

This method is intended to be overriden by clients and should terminate
any checking process that has been started.  It is called by the method
that sends the ping response if the check is not finished sufficiently
quickly.

=cut

sub ping_kill_action {
  my $self = shift;
  # nothing to kill
  return 1;
}

=head2 C<ping_check()>

This method is the used to perform any checks needed to confirm that the
client is functioning correctly.  It calls L<ping_start> to record the
time and then calls L<ping_action>.

=cut

sub ping_check {
  my $self = shift;
  $self->ping_start();
  $self->ping_action();
  return 1;
}

=head2 C<ping_start()>

This method is the used to record the start time of the ping checking.

=cut

sub ping_start {
  my $self = shift;
  $self->{ping} =
    {
     start => Time::HiRes::time,
    };
  return 1;
}

=head2 C<ping_done()>

This method is the used to record the end time and status of the ping
checking.  The default status is 'ok'.

=cut

sub ping_done {
  my $self = shift;
  $self->{ping}->{state} = shift || 'ok';
  my $end = $self->{ping}->{end} = Time::HiRes::time;
  $self->{ping}->{time} = $end - $self->{ping}->{start};
  return 1;
}

=head2 C<send_ping_response()>

This method is the used to determine if the ping checking actions have
succeeded and to send the response.  Or if the ping checking is still
running to terminate it and send a C<ping.response> with state
'timeout'.

=cut

sub send_ping_response {
  my $self = shift;
  my $delay = shift;
  my %body =
    (
     delay => $delay,
     state => $self->{ping}->{state} || 'timeout',
    );
  $body{checktime} = $self->{ping}->{time} if (exists $self->{ping}->{time});
  $self->send(class => 'ping.response',
              body => \%body);
  return 1;
}

=head1 COMMON MESSAGE METHODS

=head2 C<send_extra_hbeat()>

This method is called when the client wants to send an extra heartbeat
message.  For example, it is used to respond to a C<hbeat.request>
message.

=cut

sub send_extra_hbeat {
  my $self = shift;
  $self->send_hbeat(@_);
  $self->reset_timer('!hbeat') if ($self->exists_timer('!hbeat'));
  return 1;
}

=head2 C<send_hbeat()>

This method is called periodically to send hbeat messages.

=cut

sub send_hbeat {
  my $self = shift;
  $self->{_hbeat_count}++;
  $self->send(class => 'hbeat.app',
              body =>
              {
               interval => $self->hbeat_interval,
               port => $self->listen_port,
               remote_ip => $self->ip,
              },
             );

  # if we are due to respond to a request but we've sent a message anyway
  # make sure we don't send another one
  $self->remove_timer('!hbeat-response')
    if ($self->exists_timer('!hbeat-response'));
  return 1;
}

=head2 C<send_hbeat_end()>

This method is called to send a dying hbeat message.

=cut

sub send_hbeat_end {
  my $self = shift;
  $self->send(class => 'hbeat.end',
              body =>
              {
               interval => $self->hbeat_interval,
               port => $self->listen_port,
               remote_ip => $self->ip,
              },
             );
  return 1;
}

1;
__END__

=head1 TODO

There are some 'todo' items for this module:

=over 4

=item Callbacks

The client should have callbacks for significant events like finding a
hub.

=back

=head1 EXPORT

None by default.

=head1 SEE ALSO

Project website: http://www.xpl-perl.org.uk/

=head1 AUTHOR

Mark Hindess, E<lt>soft-xpl-perl@temporalanomaly.comE<gt>

=head1 COPYRIGHT

Copyright (C) 2005, 2008 by Mark Hindess

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.7 or,
at your option, any later version of Perl 5 you may have available.

=cut
