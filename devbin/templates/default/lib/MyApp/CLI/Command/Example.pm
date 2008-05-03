package MyApp::CLI::Command::Example;
use base qw/MyApp::CLI::Command/;

=head1 NAME
MyApp::CLI::Command::List - list events
=cut

sub opt_desk {
    return (
        ["open", "only unfinished events"],
    );
}

sub validate_args {
    my ($self, $opt, $args) = @_;
    # we need at least one argument beyond the options
    die $self->usage->text unless @$args;
}

sub run {
    my ($self, $opt, $args) = @_;
    use Data::Dumper;
    warn Dumper $self->schema;
    warn Dumper $self->config;
}

1;


