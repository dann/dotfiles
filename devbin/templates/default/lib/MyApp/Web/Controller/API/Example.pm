package MyApp::Web::Controller::API::Example;

use strict;
use warnings;
use base 'MyApp::Web::Controller::API';

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut


=head2 index

=cut
sub list {
    my ($self, $c) = @_;

    # Implement me!

    $self->status_ok(
            $c,
            entity => {
                some => 'data',
                foo  => 'is real bar-y',
            },
    );

}

# POST /articles
sub create {
    my ($self, $c) = @_;
    my $article_data = $c->req->data;
    use Data::Dumper;
    warn Dumper $article_data;
    # Implement me!
}

# GET /articles/{article_id}
sub show {
    my ($self, $c, $article_id) = @_;
}

# PUT /articles/{article_id}
sub update {
    my ($self, $c, $article_id) = @_;
}

# DELETE /articles/{article_id}
sub destroy {
    my ($self, $c, $article_id) = @_;
}

# GET /articles/new
sub post {
    my ($self, $c) = @_;
}

# GET /articles/{article_id}/edit
sub edit {
    my ($self, $c, $article_id) = @_;
}


=head1 AUTHOR

dann,,,

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
