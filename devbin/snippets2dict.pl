#!/usr/bin/perl
use strict;
use warnings;

use Carp;
use File::Find;

my $langtype = $ARGV[0];
unless($langtype) {
    die "langtype is required. Usage: snippets2dict.pl perl";
}

my $dir = "$ENV{'HOME'}/.vim/after/ftplugin";
unless(-d $dir) {
    die "$dir isn't exist"
}

find(\&print_file, $dir);

sub print_file {
    if($File::Find::name =~ /([a-zA-Z0-9\-]+)_snippets\.vim$/) {
        my $filetype = $1;

        my $file_path = $File::Find::name;
        my @contents = get_contents($file_path);
        foreach my $line (@contents) {
            if($line =~/Snippet\s+(.+?)\s+/i) {
                if($langtype eq $filetype) {
                    print $1 . "\n";
                }
            }
        }
    }
}

sub get_contents {
    my $file = shift;

    my $fh;
    open $fh, '<', $file or Carp::confess( "unable to open $file: $!" );

    if (wantarray) {
        my @contents = <$fh>;
        close $fh;
        return @contents;
    }

    my $contents = do { local $/; <$fh> };
    close $fh;
    return $contents;
}


