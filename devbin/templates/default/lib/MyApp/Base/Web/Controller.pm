package MyApp::Base::Web::Controller;
use base 'Catalyst::Controller';

# Note that 'auto' runs after 'begin' but before your actions and that
# 'auto' "chain" (all from application path to most specific class are run)
# See the 'Actions' section of 'Catalyst::Manual::Intro' for more info.
sub auto : Private {
    my ( $self, $c ) = @_;

    # Allow unauthenticated users to reach the login page.  This
    # allows anauthenticated users to reach any action in the Auth
    # controller.  To lock it down to a single action, we could use:
    #   if ($c->action eq $c->controller('Login')->action_for('index'))
    # to only allow unauthenticated access to the C<index> action we
    # added above.
    if ( $c->controller eq $c->controller('Auth') ) {
        return 1;
    }

    # If a user doesn't exist, force login
    if ( !$c->user_exists ) {

        # Dump a log message to the development server debug output
        $c->log->debug('***User not found, forwarding to /login');

        # Redirect the user to the login page
        $c->response->redirect( $c->uri_for('/auth/login') );

        # Return 0 to cancel 'post-auto' processing and prevent use of application
        return 0;
    }

    $c->log->debug('***User found');
    # User found, so return 1 to continue with processing after this 'auto'
    return 1;
}

sub service: Private {
    my ($self, $service_name) = @_;
    return MyApp::ServiceContainer->get($service_name);
}

1;
