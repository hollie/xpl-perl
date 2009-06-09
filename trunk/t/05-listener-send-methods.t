#!/usr/bin/perl -w
#
# Copyright (C) 2005, 2007 by Mark Hindess

use strict;
use Test::More tests => 11;
use t::Helpers qw/test_error test_warn/;
use Socket;
use Time::HiRes;
$|=1;

use_ok('xPL::Listener');

my $xpl = xPL::Listener->new(interface => 'lo');
ok($xpl, 'listener');

my $expected_message =
'xpl-cmnd
{
hop=1
source=bnz-tester.default
target=*
}
osd.basic
{
command=write
text=This is a test
}
';

ok($xpl->send_from_string('-c osd.basic -s bnz-tester.default '.
                          'command=write text="This is a test"'),
   'send_from_string');

is($xpl->last_sent_message(),
   $expected_message,
   'send_from_string - content');

ok($xpl->send_from_arg_list('-m', 'xpl-cmnd',
                            '-c', 'osd.basic', '-s', 'bnz-tester.default',
                            'command=write', 'text=This is a test'),
   'send_from_arg_list');

is($xpl->last_sent_message(),
   $expected_message,
   'send_from_arg_list - content');

$expected_message =~ s/\*/bnz-tester.test1/;
ok($xpl->send_from_arg_list('-m', 'xpl-cmnd', '-t', 'bnz-tester.test1',
                            '-c', 'osd.basic', '-s', 'bnz-tester.default',
                            'command=write', 'text=This is a test'),
   'send_from_arg_list w/target');

is($xpl->last_sent_message(),
   $expected_message,
   'send_from_arg_list w/target - content');

is(test_error(sub {
     $xpl->send_from_string('-c osd.basic command=write text="This is a test"')
   }),
   'xPL::Listener->send_aux: message error: xPL::Message::osd::basic::xplcmnd'.
     "->parse_head_parameters: requires 'source' parameter",
   'missing source');

is(test_error(sub {
     $xpl->send_from_string('-s bnz-tester.default '.
                            'command=write text="This is a test"')
   }),
   'xPL::Listener->send_aux: message error: '.
     "xPL::Message->new: requires 'class' parameter",
   'missing class.type');

is(join("!!",xPL::Listener::simple_tokenizer('test=1 2 3 ~ stop')),
   'test!!1!!2!!3!!~ stop',
   'tokenizer stops on invalid characters');