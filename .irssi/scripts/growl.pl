#!/usr/bin/env perl -w
#
# This is a simple irssi script to send out Growl notifications using
# Mac::Growl. Currently, it sends notifications when your name is
# highlighted, and when you receive private messages.
#

# Mac::Growlがどうやっても文字化けするので、Net::Growl経由にした
# by cohtan

use strict;
use vars qw($VERSION %IRSSI $Notes $AppName);

use Irssi;
use Net::Growl;
use Encode;

my $password = ''; # Growlにパスワード設定する

$VERSION = '0.05';
%IRSSI = (
    authors     =>  'Nelson Elhage',
    contact     =>  'Hanji@users.sourceforge.net',
    name        =>  'growl',
    description =>  'Sends out Growl notifications for Irssi',
    license     =>  'BSD',
    url         =>  'http://growl.info/',
);

sub cmd_growl ($$$) {
    Irssi::print('%G>>%n Growl can be configured using three settings:');
    Irssi::print('%G>>%n growl_show_privmsg : Notify about private messages.');
    Irssi::print('%G>>%n growl_show_hilight : Notify when your name is hilighted.');
    Irssi::print('%G>>$h growl_show_notify : Notify when someone on your away list joins or leaves.');  
}

$Notes = ["Script message", "Message notification"];
$AppName = "irssi";

# Mac::Growl::RegisterNotifications($AppName, $Notes, $Notes);
Net::Growl::register (
    host => 'localhost',
    application => $AppName,
    password => $password
);

sub sig_message_private ($$$$) {
    return unless Irssi::settings_get_bool('growl_show_privmsg');

    my ($server, $data, $nick, $address) = @_;

    # Mac::Growl::PostNotification($AppName, "Message notification", "$nick", "$data");
    notify (
        application => $AppName,
        title       => Encode::decode('utf8', $nick),
        description => Encode::decode('utf8', $data),
        priority    => 2,
        sticky      => 'True',
        password    => $password
    );
}

sub sig_print_text ($$$) {
    return unless Irssi::settings_get_bool('growl_show_hilight');

    my ($dest, $text, $stripped) = @_;

    if ($dest->{level} & MSGLEVEL_HILIGHT) {
        # Mac::Growl::PostNotification($AppName, "Message notification", $dest->{target}, $stripped);
        notify (
            application => $AppName,
            title       => Encode::decode('utf8', $dest->{target}),
            description => Encode::decode('utf8', $stripped),
            priority    => 2,
            sticky      => 'True',
            password    => $password
        );
    }
}

sub sig_notify_joined ($$$$$$) {
    return unless Irssi::settings_get_bool('growl_show_notify');
    my ($server, $nick, $user, $host, $realname, $away) = @_;
    
    # Mac::Growl::PostNotification($AppName, "Message notification", $realname || $nick,
    #    "<$nick!$user\@$host>\nHas joined $server->{chatnet}");

    notify (
        application => $AppName,
        title       => Encode::decode('utf8', $realname || $nick),
        description => Encode::decode('utf8', "<$nick!$user\@$host>\nHas joined $server->{chatnet}"),
        priority    => 2,
        sticky      => 'True',
        password    => $password
    );
}

sub sig_notify_left ($$$$$$) {
    return unless Irssi::settings_get_bool('growl_show_notify');
    my ($server, $nick, $user, $host, $realname, $away) = @_;
    
    # Mac::Growl::PostNotification($AppName, "Message notification", $realname || $nick,
    #    "<$nick!$user\@$host>\nHas left $server->{chatnet}");   
    notify (
        application => $AppName,
        title       => Encode::decode('utf8', $realname || $nick),
        description => Encode::decode('utf8', "<$nick!$user\@$host>\nHas joined $server->{chatnet}"),
        priority    => 2,
        sticky      => 'True',
        password    => $password
    );
}

Irssi::command_bind('growl', 'cmd_growl');

Irssi::signal_add_last('message private', \&sig_message_private);
Irssi::signal_add_last('print text', \&sig_print_text);
Irssi::signal_add_last('notifylist joined', \&sig_notify_joined);
Irssi::signal_add_last('notifylist left', \&sig_notify_left);

Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_privmsg', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_hilight', 1);
Irssi::settings_add_bool($IRSSI{'name'}, 'growl_show_notify', 1);

Irssi::print('%G>>%n '.$IRSSI{name}.' '.$VERSION.' loaded (/growl for help)');

