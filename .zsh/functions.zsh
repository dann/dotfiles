# ------------------------
# Dev common
# ------------------------
grep-find () { find . -type f -print0 | xargs -0 -e grep -n --binary-files=without-match -e $@ | grep -E -v \(\*.\*~\|tags\) }

# show all history
function history-all { history -E 1 }

function ssh_screen(){
    A=$#
    eval server=$"$A"
    screen -t $server ssh "$@"
}

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
    case $1 in
        *.tar.gz|*.tgz) tar xzvf $1 ;;
        *.tar.xz) tar Jxvf $1 ;;
        *.zip) unzip $1 ;;
        *.lzh) lha e $1 ;;
        *.tar.bz2|*.tbz) tar xjvf $1 ;;
        *.tar.Z) tar zxvf $1 ;;
        *.gz) gzip -dc $1 ;;
        *.bz2) bzip2 -dc $1 ;;
        *.Z) uncompress $1 ;;
        *.tar) tar xvf $1 ;;
        *.arj) unarj $1 ;;
    esac
}
alias -s {gz,tgz,zip,lzh,bz2,tbz,Z,tar,arj,xz}=extract

#-------------------------------
# Perl
#-------------------------------

# set PERL5LIB env
perl5lib () {
    export PERL5LIB="$PWD/lib:$PWD/t/lib:$PWD/t/*/lib:/opt/local/lib/perl5/site_perl/5.8.8/darwin-2level:${HOME}/perl5/perlbrew/perls/current/lib/5.12.1/lib:${HOME}/perl5/perlbrew/perls/current/lib/site_perl/5.12.1:${PERL5LIB}"

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
refe_utf8() {
  refe $@ | nkf -Ew
}
alias refe='refe_utf8'

function cdrake () {
    cdf "Rakefile"
}


