#!/usr/bin/perl

use strict;
use warnings;

use Pod::Usage;
use Getopt::Long;
use Perl6::Say;

use ExtUtils::MakeMaker qw(prompt);

use Text::MicroMason;
use Path::Class qw/dir file/;
use File::Copy;
use File::HomeDir;
use YAML;
use Data::Dumper;
use UNIVERSAL::require;
sub p ($) { print Dumper shift }

use Cwd;

sub cd ($&) {
    my ( $dir, $block ) = @_;
    my $cd = cwd();
    chdir $dir;
    my $ret = $block->();
    chdir $cd;
    return $ret;
}

GetOptions( \my %opts, qw/help/ );

$opts{help} && pod2usage(0);

exit unless @ARGV ==1;

my $conf_path = File::Spec->catfile( $ENV{HOME}, "/.catstarter" );
my $config = init_config($conf_path);
check_dependencies();

mk_app();

sub mk_app {
    my $template = select_templates();
    if ($template) {
        say "Selected $template";
    }
    else {
        say "No template selected";
        exit 1;
    }

    my $module    = shift @ARGV;
    my $pkg       = [ split /::/, $module ];
    my $dist      = join "-", @$pkg;
    my $path      = join( "/", @$pkg ) . ".pm";
    my $appprefix = Catalyst::Utils::appprefix($module);
    my $author    = $config->{author};
    my $email     = $config->{email};

    copy_templates_to_dist(
        $template,
        $dist,
        {   rule => {
                MyApp => $module,
                myapp => $appprefix,
            },
            vars => {
                '$module'    => $module,
                '$pkg'       => $pkg,
                '$dist'      => $dist,
                '$path'      => $path,
                '$module'    => $module,
                '$appprefix' => $appprefix,
                '$author'    => $author,
                '$email'     => $email,
            },
        }
    );
    make_scripts_executable($dist);
}

sub init_config {
    my $conf_path = shift;
    my $config = eval { YAML::LoadFile($conf_path) } || {};

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
    YAML::DumpFile( $conf_path, $config ) if $save;
    return $config;
}

sub check_dependencies {
    print qq/Checking dependencies ... \n/;
    for my $lib ( @{ $config->{dependencies} } ) {
        $lib->require;
    }
}

sub copy_templates_to_dist {
    my ( $template_dir, $dist, $opts ) = @_;
    my $rule = $opts->{rule};
    my $vars = $opts->{vars};

    my $mason = Text::MicroMason->new(qw/ -ServerPages -AllowGlobals /);
    $mason->set_globals(%$vars);

    $template_dir = dir($template_dir)->absolute;
    $dist         = dir($dist);

    $template_dir->recurse(
        callback => sub {
            my ($file) = @_;

            my $path = $file->relative($template_dir);

            for my $exclude_rule ( @{ $config->{exclude_rules} } ) {
                if ( $path =~ /$exclude_rule/ ) {
                    return;
                }
            }

            if ( !$file->is_dir ) {
                for my $key ( keys %$rule ) {
                    $path =~ s/$key/$rule->{$key}/ge;
                }
                $path =~ s/^$template_dir/$dist/;
                $path = file($path);

                my $target = $dist->file($path);

                $target->dir->mkpath(1);
                say "$target <- $file";
                copy( $file, $target );


                my $content = $target->slurp;
                for my $key ( keys %$rule ) {
                    $content =~ s/$key/$rule->{$key}/ge;
                }

                # FIXME
                if ( $path =~ /\.erb/ ) {
                    return;
                }
                my $fh = $target->openw;
                print $fh $mason->execute( text => $content );
                $fh->close;
            }
        }
    );
}

sub select_templates {
    my $global = file(__FILE__)->dir->absolute->subdir('templates');
    my $local
        = dir( File::HomeDir->my_home )->subdir( '.catstarter', 'templates' );
    say "Global templates: $global";
    say "Local templates: $local";

    my $templates = [];

    for my $dir ( $local, $global ) {
        ( -e "$dir" ) || next;

        my $tmpls = [];
        for my $f ( $dir->children ) {
            next unless $f->is_dir;

            $f = file($f);


            if ( $f->basename eq 'default' ) {
                unshift @$tmpls, file($f);
            }
            else {
                unless ($f->basename =~/\.svn/) {
                    push @$tmpls, file($f);
                }
            }
        }

        push @$templates, @$tmpls;
    }

    for ( 1 .. @$templates ) {
        my $template = $templates->[ $_ - 1 ];
        my $dispname = $template->basename;
        $dispname .= ' (.catstarter)' if $template =~ /\.catstarter/;
        say sprintf "[%d]: %s", $_, $dispname;
    }
    my $selected = prompt( 'Select:', '1' );

    $templates->[ $selected - 1 ]->cleanup;
}

sub make_scripts_executable {
    my $dist = shift;
    !system "chmod 755 ./$dist/script/*" or die $?;
}

__END__

