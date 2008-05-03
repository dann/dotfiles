package MyApp::Web;
use strict;
use warnings;

use Catalyst::Runtime '5.70';
use Catalyst qw(
    ConfigLoader
    FormValidator::Simple
    FormValidator::Simple::Auto
);

our $VERSION = '0.01';

__PACKAGE__->config( 'Plugin::ConfigLoader' => { file => __PACKAGE__->path_to('conf', 'myapp.yml') } );

__PACKAGE__->setup(
    do {
        my @plugins = qw/
            FillInForm::ForceUTF8
            Authentication
            Session
            Session::Store::FastMmap
            Session::State::Cookie
            Email::Japanese
            Unicode
            I18N
            RequestToken
            BuildURI
            FormValidator::Simple
            FormValidator::Simple::Auto
        /;

        push @plugins, 'StackTrace' if $ENV{CATALYST_DEBUG};
        push @plugins, 'Log::Colorful' if $ENV{CATALYST_DEBUG};
        push @plugins, 'Static::Simple' if $ENV{CATALYST_ENGINE} =~ /^HTTP/;
        @plugins;
    }
);

1;
