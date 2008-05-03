package MyApp::Web::Controller::Auth;
use base 'MyApp::Base::Web::Controller';
use strict;
use warnings;

use MyApp::Exception;

=head1 NAME

MyApp::Web::Controller::Auth - Catalyst Controller

=head1 DESCRIPTION

Catalyst Controller.

=head1 METHODS

=cut

=head2 index 

=cut

sub index : Private {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'auth/login.tt2';
}

sub login : Local {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'auth/login.tt2';
}

sub do_login : Local {
    my ( $self, $c ) = @_;
    return $c->forward('index') if $c->form->has_error;

    try {
        if ($c->authenticate(
                {   username => $c->request->params->{'username'},
                    password => $c->request->params->{'password'}
                }
            )
            )
        {
            # FIXME
            $c->response->redirect( $c->uri_for('/') );

        }
        else {
            # FIXME
            $c->detach('error');
        }
    }
    except {
        my $E = shift;
        $c->log->error( $E->{-description} );
        $c->detach('error');
    }
}

sub preferences : Local {
    my ( $self, $c ) = @_;

    if ( $c->user_exists() ) {
        ## If a user is logged in - you can show them the preferences page.
        $c->stash->{'template'} = 'preferences.tt2';
    }
    else {
        # otherwise bounce them to the login page.
        $c->response->redirect( $c->uri_for('/auth/login') );
    }
}

sub logout : Local {
    my ( $self, $c ) = @_;
    $c->logout();
    $c->response->redirect( $c->uri_for('/') );
}

sub error : Local {
    my ( $self, $c ) = @_;
    $c->stash->{template} = 'auth/error.tt';
}

=head1 AUTHOR

A clever guy

=head1 LICENSE

This library is free software, you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

1;
