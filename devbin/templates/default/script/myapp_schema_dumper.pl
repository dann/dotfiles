#!/usr/bin/env perl

use strict;
use warnings;

use YAML;
use FindBin;
use File::Spec;
use lib File::Spec->catfile( $FindBin::Bin, qw/.. schema lib/ );

use Path::Class qw/file dir/;
use DBIx::Class::Schema::Loader qw/make_schema_at/;

# do manual delete instead of really_erase_my_files option
#    for keep MyApp/Schema.pm
my $libdir = dir($FindBin::Bin, '..', 'lib', 'MyApp', 'Schema' );
if (-d $libdir) {
    $libdir->rmtree;
}

my $config = YAML::LoadFile(File::Spec->catfile( $FindBin::Bin, '..', 'conf', 'myapp.yml'));
my $db_config = $config->{'Model::DBIC'};

make_schema_at(
    $db_config->{schema_class},
    {   components => [ 'ResultSetManager', 'UTF8Columns', 'InflateColumn::DateTime' ],
        dump_directory => File::Spec->catfile( $FindBin::Bin, '..', 'lib' ),
        debug          => 1,
    },
    $db_config->{'connect_info'},
);

1;
