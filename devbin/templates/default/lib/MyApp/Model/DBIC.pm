package MyApp::Model::DBIC;
use MyApp::ConfigLoader;
use DBIx::Class::QueryLog;
use strict;
use warnings;

BEGIN {
    use base qw/MyApp::Base::Model/;
    use UNIVERSAL;
    use MyApp::Exception qw/:try/;

    __PACKAGE__->mk_group_accessors('component_class', qw/schema_class/);
    __PACKAGE__->mk_group_accessors('inherited', qw/
        config
        connection_info
        _resultset
        _schema
        querylog
    /);
};
__PACKAGE__->schema_class('MyApp::Schema');

sub new {
    my $class = shift;
    my $self  = $class->SUPER::new(@_);
    $self->setup_config;
    $self->setup_connection_info;
    $self->generate_classes;
    return $self;
}

sub setup_config {
    # FIXME
    # Catalystで使うときは、configloaderがセットされた状態にする。
    my ($self) = @_;
    unless($self->config) {
        my $config = MyApp::ConfigLoader->new->config;
        $self->config($config);
    }
}

sub setup_connection_info {
    my ($self) = @_;
    $self->connection_info(
        $self->config->{'Model::DBIC'}{'connect_info'}
    );
}

sub generate_classes {
    my ($self) = @_;
    foreach my $source ( $self->schema->sources ) {
        no strict 'refs';
        my $class = "MyApp::Model::DBIC::$source";
        push @{"$class\::ISA"}, "MyApp::Model::DBIC";

        no strict 'refs';
        *{"$class\::source_name"} = sub {
            use strict;
            my ( $self, %args ) = @_;
            return "$source";
        };
    }
}

sub resultset {
    my ($self, $resultset) = @_;

    if (defined $resultset) {
        $self->_resultset($resultset);
    } elsif (!$self->_resultset) {
        if (!$self->source_name) {
            MyApp::Exception->throw('SCHEMA_SOURCE_NOT_SPECIFIED');
        };

        try {
            $self->_resultset($self->schema->resultset($self->source_name));
        } except {
            MyApp::Exception->throw('SCHEMA_SOURCE_NOT_FOUND', $self->source_name);
        };
    };

    return $self->_resultset;
};

sub schema {
    my ($self, $schema) = @_;

    if ($schema) {
        $self->_schema($schema);
    } elsif (!$self->_schema) {
        if (!$self->schema_class) {
            MyApp::Exception->throw('SCHEMA_CLASS_NOT_SPECIFIED');
        };
        my $schema = $self->schema_class->connect(@{$self->connection_info || []});
        # FIXME
        if(1) {
            my $ql = new DBIx::Class::QueryLog();
            $schema->storage->debugobj($ql);
            $schema->storage->debug(1);
            $self->querylog($ql);
        }
        $self->_schema($schema);
    };

    return $self->_schema;
};

sub source_name {
    die "Virtual Method";
}

sub DESTROY {}

sub AUTOLOAD {
    my $self = shift;
    my $method = our $AUTOLOAD;
    $method =~ s/.*:://o;
    warn $method;
    $self->resultset->$method(@_);
}

1;
