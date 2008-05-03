#!/usr/bin/env perl

use strict;
use warnings;

use YAML;
use ExtUtils::MakeMaker qw(prompt);
use Template;
use File::Spec;
use File::Path;
use Path::Class;
use IO::All;
use Getopt::Long;
use Pod::Usage;
use UNIVERSAL::require;
GetOptions( \my %opt, qw/help/, 'skel=s' );
pod2usage(2) if $opt{help};

my $path = File::Spec->catfile( $ENV{HOME}, "/.catsetuprc" );
my $config = eval { YAML::LoadFile($path) } || {};

my $save;
while ( !$config->{author} ) {
    $config->{author} = prompt( "Your name: ", '' );
    $save++;
}

while ( !$config->{email} ) {
    $config->{email} = prompt( "Your email: ", '' );
    $save++;
}

my @default_exclude_rules = qw/\.svn \.cvs/;
unless ( $config->{exclude_rules} ) {
    $config->{exclude_rules} = \@default_exclude_rules;
    $save++;
}

unless ( $config->{dependencies} ) {
    my @default_dependencies = qw/Catalyst/;
    $config->{dependencies} = \@default_dependencies;
    $save++;
}

check_dependencies();

my $skelton_dir    = skelton_dir();

pod2usage(2) unless $skelton_dir;

my $app_name       = shift @ARGV;      # MyApp
my $lower_app_name = lc($app_name);    # myapp
my $upper_app_name = uc($app_name);    # MYAPP
my $app_dir        = $app_name;
my $variables = {
    app_name       => $app_name,
    lower_app_name => $lower_app_name,
    upper_app_name => $upper_app_name,
    author         => $config->{author},
    email          => $config->{email},
};

mk_app();

END {
    YAML::DumpFile( $path, $config ) if $save;
}

sub skelton_dir {
    if ( !$opt{skel}
        && -d "$ENV{HOME}/.catsetup/skelton/default" )
    {
        return "$ENV{HOME}/.catsetup/skelton/default";
    }
    if ( $opt{skel} && -d "$ENV{HOME}/.catsetup/skelton/$opt{skel}" ) {
        return "$ENV{HOME}/.catsetup/skelton/$opt{skel}";
    }
    return;
}

sub check_dependencies {
    print qq/Checking dependencies ... \n/;
    for my $lib ( @{ $config->{dependencies} } ) {
        $lib->require;
    }
}

sub mk_app {
    mk_app_dir();
    my ( $all_files, $all_dirs ) = collect_skelton_dir_and_files();
    mk_module_dirs($all_dirs);
    render_templates($all_files);
    make_scripts_executable();
}

sub mk_app_dir {
    mk_dir($app_dir);
}

sub mk_module_dirs {
    my $dirs = shift;
    for my $dir (@$dirs) {
        my $to_dir = to_file_path($dir);
        mk_dir($to_dir);
    }
}

sub collect_skelton_dir_and_files {
    my @all_files = ();
    my @all_dirs  = ();
    dir($skelton_dir)->recurse(
        callback => sub {
            my $file = shift;

            for my $exclude_rule ( @{ $config->{exclude_rules} } ) {
                if ( $file =~ /$exclude_rule/ ) {
                    return;
                }
            }

            if ( -f $file ) {
                $file =~ s/$skelton_dir//;
                $file =~ s/^\///;
                push @all_files, $file;
            }

            if ( -d $file ) {
                $file =~ s/$skelton_dir//;
                $file =~ s/^\///;
                push @all_dirs, $file;
            }
        }
    );

    return ( \@all_files, \@all_dirs );
}

sub render_templates {
    my $templates = shift;
    my $tt        = Template->new(
        { INCLUDE_PATH => $skelton_dir, TAG_STYLE => 'star' } );
    for my $t (@$templates) {
        my $to_file = to_file_path($t);
        my $output;
        $tt->process( $t, $variables, \$output )
            || die $tt->error();
        $output > io($to_file);
        print qq/generated "$to_file"\n/;
    }
}

sub to_file_path {
    my $original_file_path = shift;
    foreach my $key ( keys %{$variables} ) {
        $original_file_path =~ s/__$key\__/$variables->{$key}/g;
    }
    return File::Spec->catfile( $app_dir, $original_file_path );
}

sub mk_dir {
    my ($dir) = @_;
    if ( -d $dir ) {
        print qq/ exists "$dir"\n/;
        return 0;
    }
    if ( mkpath $dir ) {
        print qq/created "$dir"\n/;
        return 1;
    }

    die "Couldn't create $dir";
}

sub make_scripts_executable {
    !system "chmod 755 ./$app_dir/script/*" or die $?;
}

=head1 SYNOPSIS

catsetup.pl MyApp

Options:
  --skel Catalystアプリのskelton
        ~/.catsetup/skelton/<skel>

  何もオプションが指定されない場合は、
  ~/.catsetup/skelton/default
  のskeltonが選択されます。

  テンプレートは ~/.catsetup/skelton/<skel> に置いてください。

=cut

=head1 DESCRIPTION

Catalstアプリの雛形から、Catalystアプリを生成するStarter
