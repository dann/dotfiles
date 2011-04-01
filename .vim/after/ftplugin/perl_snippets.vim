if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

"============================================
" Perl 
"============================================
exec "Snippet snsub sub ".st."FunctionName".et." {<CR>my $self=shift;".st.et."<CR>}<CR>".st.et
exec "Snippet snsubn sub ".st."FunctionName".et." {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snxfore ".st."expression".et." foreach @".st."array".et.";".st.et
exec "Snippet snxwhile ".st."expression".et." while ".st."condition".et.";".st.et
exec "Snippet snxunless ".st."expression".et." unless ".st."condition".et.";".st.et
exec "Snippet snslurp my $".st."var".et.";<CR><CR>{ local $/ = undef; local *FILE; open FILE, \"<".st."file".et.">\"; $".st."var".et." = <FILE>; close FILE }".st.et
exec "Snippet snif if (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snunless unless (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snifee if (".st.et.") {<CR>".st.et."<CR><BS>} elsif (".st.et.") {<CR>".st.et."<CR><BS>} else {<CR>".st.et."<CR>}<CR><CR>".st.et
exec "Snippet snife if (".st.et.") {<CR>".st.et."<CR>} else {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snfor for (my \$".st."var".et." = 0; \$".st."var".et." < ".st."expression".et."; \$".st."var".et."++) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snfore foreach my \$".st."var".et." (@".st."array".et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet sneval eval {<CR>".st.et."<CR>};<CR>if ($@) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snwhile while (".st.et.") {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snxif ".st."expression".et." if ".st."condition".et.";".st.et

exec "Snippet snclass package ".st."ClassName".et.";<CR><CR>use base qw(Class::Accessor::Fast);<CR>__PACKAGE__->mk_accessors(qw/".st.et."/);<CR><CR>sub new {<CR>my \$class = shift;<CR>\$class = ref \$class if ref \$class;<CR>my $self = bless {<CR>}, \$class;<CR>\return $self;<CR>}<CR><CR>1;<CR>".st.et


exec "Snippet sndump use Data::Dumper; warn Dumper ".st."var".et.";<CR>".st.et
exec "Snippet snsay print ".st."var".et.", \"\\n\";<CR>".st.et
exec "Snippet snself my \$self = shift;<CR>".st.et
exec "Snippet sndata my \$data = do { local $/; <DATA> };<CR>".st.et
exec "Snippet snargf while (<>) {<CR>chomp;<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snisa if (blessed $".st."var".et." and $".st."var".et."->isa('".st."Class".et."')) {<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snreadtsv use IO::File;<CR>use Text::CSV_XS;<CR><CR>my \$fh = IO::File->new('".st."filename".et."') or die 'cannot open file.';<CR>my \$csv = Text::CSV_XS->new({ sep_char => \"\\t\", binary => 1 });<CR>until ($fh->eof) {<CR>my $cols = $csv->getline($fh);<CR>unless ($cols) {<CR>warn $csv->error_diag;<CR>next;<CR>}<CR>my (".st.et.") = @$cols;<CR>}<CR>$fh->close;<CR>".st.et

" Class-C3
exec "Snippet snnext $self->next::method(@_);<CR>".st.et
exec "Snippet snmaybe $self->maybe::next::method(@_);<CR>".st.et


"============================================
" Catalyst
"============================================
exec "Snippet sndebug $c->log->debug('".st."name".et.": '. \$".st."var".et.");<CR>".st.et
exec "Snippet snwarn $c->log->warn('".st."name".et.": '. \$".st."var".et.");<CR>".st.et
exec "Snippet sndumper $c->log->dumper('".st."name".et.": '. \$".st."var".et.");<CR>".st.et
exec "Snippet snmodel $c->model('".st."model".et."')".st.et
exec "Snippet snview $c->view('".st."model".et."')".st.et
exec "Snippet snconfig $c->config->{".st."name".et."}".st.et
exec "Snippet sncontroller sub ".st."func".et." : ".st."Attribute".et." {<CR>my ($self, $c) = @_;<CR>".st.et."<CR>}<CR>".st.et
exec "Snippet snbegin sub begin : Private {<CR>my ($self, $c) = @_;<CR>".st.et."<CR>1;<CR>}<CR>".st.et
exec "Snippet snauto sub auto : Private {<CR>my ($self, $c) = @_;<CR>".st.et."<CR>1;<CR>}<CR>".st.et
exec "Snippet sndetach $c->detach('".st."name".et."');<CR>".st.et
exec "Snippet snforward $c->forward('".st."name".et."');<CR>".st.et
exec "Snippet snstash $c->stash->{".st."var".et."}".st.et
exec "Snippet snflash $c->flash->{".st."var".et."}".st.et
exec "Snippet snsession $c->session->{".st."var".et."}".st.et
exec "Snippet snsstash $c->stash->{".st."var".et."} = ".st.et.";<CR>".st.et
exec "Snippet snsflash $c->flash->{".st."var".et."} = ".st.et.";<CR>".st.et
exec "Snippet snssession $c->session->{".st."var".et."} = ".st.et.";<CR>".st.et
exec "Snippet snrs $c->model('DBIC::".st."Source".et."')".st.et
exec "Snippet snredirect $c->res->redirect($c->uri_for('".st."uri".et."'));<CR>".st.et
exec "Snippet snparam $c->req->param('".st."param".et."')".st.et


