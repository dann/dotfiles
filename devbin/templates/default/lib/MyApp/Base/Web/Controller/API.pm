package MyApp::Base::Web::Controller::API;
use strict;
use warnings;
use Params::Validate qw(SCALAR OBJECT);
use base 'Catalyst::Controller::Resources';

__PACKAGE__->config(
    'stash_key' => 'entity',
    'default'   => 'text/x-json',
    'map'       => {
        'text/html'          => 'YAML::HTML',
        'text/xml'           => 'XML::Simple',
        'text/x-yaml'        => 'YAML',
        'text/x-json'        => 'JSON',
    }
);

sub begin : ActionClass('Deserialize') {
}

sub end : ActionClass('Serialize') {
}


=item status_ok

Returns a "200 OK" response.  Takes an "entity" to serialize.

Example:

  $self->status_ok(
    $c,
    entity => {
        radiohead => "Is a good band!",
    }
  );

=cut

sub status_ok {
    my $self = shift;
    my $c    = shift;
    my %p    = Params::Validate::validate( @_, { entity => 1, }, );

    $c->response->status(200);
    $self->_set_entity( $c, $p{'entity'} );
    return 1;
}

=item status_created

Returns a "201 CREATED" response.  Takes an "entity" to serialize,
and a "location" where the created object can be found.

Example:

  $self->status_created(
    $c,
    location => $c->req->uri->as_string,
    entity => {
        radiohead => "Is a good band!",
    }
  );

In the above example, we use the requested URI as our location.
This is probably what you want for most PUT requests.

=cut

sub status_created {
    my $self = shift;
    my $c    = shift;
    my %p    = Params::Validate::validate(
        @_,
        {
            location => { type     => SCALAR | OBJECT },
            entity   => { optional => 1 },
        },
    );

    my $location;
    if ( ref( $p{'location'} ) ) {
        $location = $p{'location'}->as_string;
    } else {
        $location = $p{'location'};
    }
    $c->response->status(201);
    $c->response->header( 'Location' => $location );
    $self->_set_entity( $c, $p{'entity'} );
    return 1;
}

=item status_accepted

Returns a "202 ACCEPTED" response.  Takes an "entity" to serialize.

Example:

  $self->status_accepted(
    $c,
    entity => {
        status => "queued",
    }
  );

=cut

sub status_accepted {
    my $self = shift;
    my $c    = shift;
    my %p    = Params::Validate::validate( @_, { entity => 1, }, );

    $c->response->status(202);
    $self->_set_entity( $c, $p{'entity'} );
    return 1;
}

=item status_bad_request

Returns a "400 BAD REQUEST" response.  Takes a "message" argument
as a scalar, which will become the value of "error" in the serialized
response.

Example:

  $self->status_bad_request(
    $c,
    message => "Cannot do what you have asked!",
  );

=cut

sub status_bad_request {
    my $self = shift;
    my $c    = shift;
    my %p    = Params::Validate::validate( @_, { message => { type => SCALAR }, }, );

    $c->response->status(400);
    $c->log->debug( "Status Bad Request: " . $p{'message'} ) if $c->debug;
    $self->_set_entity( $c, { error => $p{'message'} } );
    return 1;
}

=item status_not_found

Returns a "404 NOT FOUND" response.  Takes a "message" argument
as a scalar, which will become the value of "error" in the serialized
response.

Example:

  $self->status_not_found(
    $c,
    message => "Cannot find what you were looking for!",
  );

=cut

sub status_not_found {
    my $self = shift;
    my $c    = shift;
    my %p    = Params::Validate::validate( @_, { message => { type => SCALAR }, }, );

    $c->response->status(404);
    $c->log->debug( "Status Not Found: " . $p{'message'} ) if $c->debug;
  $self->_set_entity( $c, { error => $p{'message'} } );
    return 1;
}

sub _set_entity {
    my $self   = shift;
    my $c      = shift;
    my $entity = shift;
    if ( defined($entity) ) {
        $c->stash->{ $self->{'stash_key'} } = $entity;
    }
    return 1;
}

1;
