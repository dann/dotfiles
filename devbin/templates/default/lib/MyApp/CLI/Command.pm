package MyApp::CLI::Command;
use base qw/App::Cmd::Command/;
use MyApp::ConfigLoader;
use MyApp::Schema;

sub config {
    my $self = shift;
    $self->{config} ||= MyApp::ConfigLoader->new->config;
    return $self->{config};
}

sub schema {
    my $self = shift;
    $app->{schema}  = MyApp::Schema->connect( @{ $self->config->{'Model::DBIC'}{'connect_info'} } );
}

1;
