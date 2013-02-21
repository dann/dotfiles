# ------------------------
# Dev common
# ------------------------
grep-find () { find . -type f -print0 | xargs -0 -e grep -n --binary-files=without-match -e $@ | grep -E -v \(\*.\*~\|tags\) }

# show all history
function history-all { history -E 1 }

# http://subtech.g.hatena.ne.jp/secondlife/20080604/1212562182
function cdf () {
    local -a tmpparent; tmpparent=""
    local -a filename; filename=""
    local -a file
    local -a num; num=0
    while [ $num -le 10 ]; do
        tmpparent="${tmpparent}../"
        file="${tmpparent}${filename}"
        if [ -f "${file}" ] || [ -d "${file}" ]; then
            cd ${tmpparent}
            break
        fi
        num=$(($num + 1))
    done
}

# Extract archive
function extract() {
    if [ -f $1 ]; then
        case $1 in
            *.Z)        uncompress $1       ;;
            *.bz2)      bunzip2 $1          ;;
            *.dmg)      hdiutil mount $1    ;;
            *.gz)       gunzip $1           ;;
            *.lzh)      lha e $1            ;;
            *.tar)      tar -xvf $1         ;;
            *.tar.bz2)  tar -jxvf $1        ;;
            *.tar.gz|*.tgz) tar xzvf $1     ;;
            *.tar.xz)   tar Jxvf $1         ;;
            *.tbz2)     tar -jxvf $1        ;;
            *.zip)      unzip $1            ;;
            *)          echo "'$1' cannot be extracted/mounted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz,dmg}=extract


function cdgit() {
  if git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
    cd `git rev-parse --show-toplevel`
  fi
}

# http://blog.kamipo.net/entry/2013/02/20/122225
function static_httpd {
  if which plackup > /dev/null; then
    plackup -MPlack::App::Directory -e 'Plack::App::Directory->new(root => ".")->to_app'
  elif which ruby > /dev/null; then
    ruby -rwebrick -e 'WEBrick::HTTPServer.new(:Port => 5000, :DocumentRoot => ".").start'
  elif which python > /dev/null; then
    if python -V 2>&1 | grep -qm1 'Python 3\.'; then
      python -m http.server 5000
    else
      python -m SimpleHTTPServer 5000
    fi
  elif which node > /dev/null; then
    node -e "var c=require('connect'), d=process.env.PWD; c().use(c.logger()).use(c.static(d)).use(c.directory(d)).listen(5000);"
  elif which php > /dev/null && php -v | grep -qm1 'PHP 5\.[45]\.'; then
    php -S 0.0.0.0:5000
  elif which erl > /dev/null; then
    erl -eval 'inets:start(), inets:start(httpd, [{server_name, "httpd"}, {server_root, "."}, {document_root, "."}, {port, 5000}])'
  fi
}


#-------------------------------
# Perl
#-------------------------------

# set PERL5LIB env
function perl5lib () {
    export PERL5LIB="$PWD/lib:$PWD/t/lib:$PWD/t/*/lib:/opt/local/lib/perl5/site_perl/5.8.8/darwin-2level:${HOME}/perl5/perlbrew/perls/current/lib/5.12.1/lib:${HOME}/perl5/perlbrew/perls/current/lib/5.12.1:${HOME}/perl5/perlbrew/perls/current/lib/site_perl/5.12.1:${HOME}/perl5/perlbrew/perls/perl-5.12.1/lib/5.12.1:$HOME/perl5/perlbrew/perls/perl-5.12.1/lib/site_perl/5.12.1/darwin-2level:${PERL5LIB}"
}

function cdmake () {
    cdf "Makefile.PL"
}


function minicpan_get () {
    perl `which minicpan` -r http://cpan.yahoo.com/ -l ~/share/minicpan
}

#-------------------------------
# Ruby 
#-------------------------------
function refe_utf8() {
  refe $@ | nkf -Ew
}
alias refe='refe_utf8'

function cdrake () {
    cdf "Rakefile"
}

#-------------------------------
# z 
#-------------------------------
_Z_CMD=j
source ~/.zsh/misc/z.sh
precmd() {
  _z --add "$(pwd -P)"
}
