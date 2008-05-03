package MyApp::Base::Model;
use strict;
use warnings;
use Moose;

BEGIN {
    use base qw/Class::Accessor::Grouped/;
};

sub new {
    my ($class, $args) = @_;
    my $self = bless {}, $class;

    $self->setup($args);

    return $self;
};

sub setup {
    my ($self, $args) = @_;

    if (ref $args eq 'HASH') {
        map {
            if ($self->can($_)) {
                $self->$_($args->{$_})
            } else {
                $self->{$_}  = $args->{$_};
            }
        } keys %{$args};
    };

    return;
};

1;
__END__
