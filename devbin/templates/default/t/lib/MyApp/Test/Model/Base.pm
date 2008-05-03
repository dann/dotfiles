package MyApp::Test::Model::Base;
use strict;
use warnings;
use base qw/Test::Class/;
use MyApp::Test::DBIC::Database;
use Test::Fixture::DBIC::Schema;
use File::Spec;

sub setup: Test(setup) {
    my $self = shift;
    $self->_make_fixture($self->model,$self->fixture);
}

sub _make_fixture {
    my $self = shift;
    my $model = shift;
    my $fixture = shift;

    $self->{schema} = MyApp::Test::DBIC::Database->connect();
    my $fixture_path = File::Spec->catfile('t','fixtures', $self->fixture . ".yaml");
    my $data = construct_fixture(
        schema  => $self->{schema},
        fixture => $fixture_path,
    );
    $self->{model} = $self->{schema}->resultset($model);
}

sub teardown : Test(teardown) {
};

sub fixture {
    my $self = shift;
    die "need to override";
}

sub model {
    my $self = shift;
    die "need to override";
}

1;
