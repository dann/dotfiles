package MyApp::Web::Logger;
use strict;
use warnings;

use Catalyst::Log::Color;
use Log::Dispatch::Config;
use Log::Dispatch::Configurator::YAML;

use Catalyst::Utils;
use NEXT;

{
    package MyApp::Web::Logger::Backend;
    use base qw/Log::Dispatch::Config/;
}

sub setup {
    my $c      = shift;
    my $class  = ref $c || $c;

    $c->log->_flush if $c->log->can('_flush');

    my $config = $c->config->{log}{config}
        || $c->path_to( Catalyst::Utils::appprefix($class) . '_log.yml' );

    MyApp::Logger::Backend->configure_and_watch(
        Log::Dispatch::Configurator::YAML->new($config) );

    $class->log( MyApp::Web::Logger::Backend->instance );
    $c->NEXT::setup(@_);
}

1;
