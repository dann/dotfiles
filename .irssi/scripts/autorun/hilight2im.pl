use strict;
use warnings;

use Irssi;
use POSIX ();

use LWP::UserAgent;
use HTTP::Request::Common;

our $VERSION = '0.1';

our %IRSSI = (
    name        => 'hilight2im',
    description => 'notify hilight message to IM via im.kayac.com api',
    authors     => 'Daisuke Murase',
);

sub sig_printtext {
    my ($dest, $text, $stripped) = @_;

    if ( $dest->{level} & MSGLEVEL_HILIGHT ) {
        my $pid = fork;
        if ($pid) {
            Irssi::pidwait_add($pid);
        }
        elsif (defined $pid) {
            my $user = Irssi::settings_get_str('im_kayac_com_username');
            LWP::UserAgent->new->request( POST "http://im.kayac.com/api/post/$user", [ message => "[irssi] $stripped" ]);
            POSIX::_exit(1);
        }
    }
}
Irssi::signal_add('print text' => \&sig_printtext);
Irssi::settings_add_str('im_kayac_com', 'im_kayac_com_username', 'username');
