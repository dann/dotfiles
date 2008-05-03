package MyApp::ConfigLoader;

use strict;
use warnings;

use MyApp::Utils;
use Config::Any;
use File::Spec;

sub new {
    my $class = shift;
    my $self = {};
    bless $self , $class;
    $self->{config} = $self->load;
    return $self;
}

# looks ugly. Fix later
sub app_name {
    'MyApp';
}

sub prefix {
    my $self = shift;
    my $prefix = MyApp::Utils::appprefix( $self->app_name );
    return $prefix;
}

sub config {
    my $self = shift;
    return $self->{config};
}

sub load {
    my $self = shift;
    my @files = $self->find_files;
    my $cfg   = Config::Any->load_files(
        {   files       => \@files,
            use_ext     => 1,
        }
    );

    my $config = {};
    my $config_local = {};
    my $local_suffix = $self->get_config_local_suffix;
    for ( @$cfg ) {

        if ( ( keys %$_ )[ 0 ] =~ m{ $local_suffix \. }xms ) {
            $config_local =  $_->{ (keys %{$_})[0] };
        }
        else {
            $config = {
                %{ $_->{ (keys %{$_})[0] }},
                %{$config} ,
            }
        }
    }

    $config = {
        %{$config},
        %{$config_local} ,
    };
    return $config;
}

sub local_file {
    my $self = shift;
    my $prefix = $self->prefix;
    return File::Spec->catfile($self->get_config_dir_path, $prefix . '_' . $self->get_config_local_suffix);
}

sub find_files {
    my $self = shift;
    my ( $path, $extension ) = $self->get_config_path;
    my $suffix     = $self->get_config_local_suffix;
    my @extensions = @{ Config::Any->extensions };

    my @files;
    if ( $extension ) {
        next unless grep { $_ eq $extension } @extensions;
        ( my $local = $path ) =~ s{\.$extension}{_$suffix.$extension};
        push @files, $path, $local;
    }
    else {
        @files = map { ( "$path.$_", "${path}_${suffix}.$_" ) } @extensions;
    }

    return @files;
}

sub get_config_dir_path {
    my $self = shift;
    my $home = MyApp::Utils->home;
    return File::Spec->catfile( $home , 'conf', $self->prefix . ".yml");

}

sub get_config_path {
    my $self = shift;
    my $path = $self->get_config_dir_path;
    my $extension = 'yml';
    return ( $path, $extension );
}

sub get_config_local_suffix {
    my $self = shift;
    my $suffix = MyApp::Utils::env_value( $self->app_name, 'CONFIG_LOCAL_SUFFIX' ) || "local";
    return $suffix;
}

1;
