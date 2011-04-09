#!/usr/bin/perl
use strict;
use warnings;

my @files = qw(
    .Xdefaults
    .bash 
    .bashrc 
    .bashrc-mac 
    .bashrc-minimum
    .bashrc_profile 
    .cgdb 
    .ctags 
    .dev_users 
    .devenvrc 
    .gdbini
    .gitconfig
    .irbrc 
    .irssi
    .my.cnf 
    .noserc
    .perltidyrc
    .pylint
    .screenrc 
    .shipit 
    .tidyrc 
    .tscreenrc 
    .vim 
    .vimrc 
    .zsh
    .zshrc
    devbin 
    devtools 
);

&main;exit;

sub main {
    (my $pwd = `pwd`) =~ s/\n//;
    for my $foo (@files) {
        my $src = "$pwd/$foo";
        my $dst = "$ENV{HOME}/$foo";
        linkit($src => $dst);
    }
}

sub linkit {
    my ($src, $dst) = @_;

    if (!-e $src) {
        print "# '$src' is missing\n";
    } elsif (-e $dst) {
        print "# $dst already exists\n";
    } else {
        print "ln -s $src $dst\n";
    }
}

