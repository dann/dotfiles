package MyApp::ServiceContainer;
use strict;
use warnings;

use Bread::Board;

our $container;
BEGIN {
    $container = container 'AppContainer' => as {

    #    service 'Model::Flickr' => (
    #        class     => 'Idolzon::Model::Flickr',
    #        lifecycle => 'Singleton',
    #    );
    #

    };
}

sub get {
    my $class= shift;
    my $service_name = shift;
    $container->fetch($service_name)->get;
}

1;
