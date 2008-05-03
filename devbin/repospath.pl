#!/usr/bin/env perl

use strict;
use warnings;

use YAML::Syck;
use Path::Class qw/file/;

my $path   = $ARGV[0] or die;

# git
if (-d "$path/.git" && -f "$path/.git/HEAD") {
    my $branch;

    my $head = file("$path/.git/HEAD")->slurp;
    chomp $head;

    if ($head =~ /^\w{40}$/) {  # no local branch?
        my $refs = qx{ grep $head -l $path/.git/refs/remotes/* };
        if ($refs =~ m!.git/refs/(\S+)!) {
            $branch = $1;
        }
    }
    elsif ($head =~ m!ref: refs/heads/(\S+)!) {
        $head = $1;
    }

    print '(', ($branch || $head), ')';
    exit;
}

# svk
if (-e "$ENV{HOME}/.svk/config") {
    my $config = YAML::Syck::LoadFile( $ENV{HOME} . '/.svk/config' );
    my $hash = $config->{checkout}{hash};
    my @path = split '/', $path;

    my $svkinfo;
    do {
        $svkinfo = $hash->{ join '/', @path }
    } while !$svkinfo and pop @path;

    exit unless $svkinfo;

    if (my ($trunk, $branch) = ($svkinfo->{depotpath} || '') =~ m!(?:/(trunk)|/(branches/[^/]+))$!) {
        print '(' . ($trunk || $branch) . ')';
    }
    elsif (my $depot = $svkinfo->{depotpath}) {
        print "($depot)";
    }
}
