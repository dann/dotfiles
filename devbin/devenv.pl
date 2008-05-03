#!/usr/bin/env perl
use strict;
use warnings;

my %default_env = (
    DEV_USERS         => get_dev_users(),
);

for my $name (keys %default_env) {
    print "export $name=$default_env{$name};" unless $ENV{$name};
}

sub get_dev_users {
    my $file = "$ENV{HOME}/.dev_users";
    my $whoami = $ENV{"USER"};
    my @users_excluded_with_me = ();
    foreach(slurp($file)) {
        chomp $_;
        if($whoami ne $_) {
            push @users_excluded_with_me, $_;
        }
    }
    return join(',',@users_excluded_with_me);
}

sub slurp {
    my $file = shift;
    open my $fh, $file or die "Could not open $file: $!\n";
    return <$fh>;
}


