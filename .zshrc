#-----------------------------------------------
# load dev env config
#-----------------------------------------------
# share screen session with users in .dev_users 
# if we load this config
[ -e ~/.devenvrc ] && source ~/.devenvrc

#-----------------------------------------------
# load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# load basic settings
#-----------------------------------------------
[ -e ~/.zshrc-minimum ] && source ~/.zshrc-minimum

export EDITOR=vim

export LESS="-erX"


function chpwd() {
    if [[ "${OSTYPE}" = darwin* ]] ; then
        gls -al --color 
    else
        ls -al
    fi
}

# disable Ctrl+S
stty stop undef

#-----------------------------------------------
# Prompot
#-----------------------------------------------
setopt prompt_subst

if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    autoload -Uz vcs_info
    zstyle ':vcs_info:(git|svn):*' formats '%R' '%S' '%b'
    zstyle ':vcs_info:(git|svn):*' actionformats '%R' '%S' '%b|%a'
    zstyle ':vcs_info:*' formats '%R' '%S' '%s:%b'
    zstyle ':vcs_info:*' actionformats '%R' '%S' '%s:%b|%a'
    precmd_vcs_info () {
        psvar=()
        LANG=en_US.UTF-8 vcs_info
        repos=`print -nD "$vcs_info_msg_0_"`
        [[ -n "$repos" ]] && psvar[2]="$repos"
        [[ -n "$vcs_info_msg_1_" ]] && psvar[3]="$vcs_info_msg_1_"
        [[ -n "$vcs_info_msg_2_" ]] && psvar[1]="$vcs_info_msg_2_"
    }
    typeset -ga precmd_functions
    precmd_functions+=precmd_vcs_info

    PROMPT="%(!.%F{red}.%F{green})%U%n@%6>>%m%>>%u%f:%1(j.%j.)%(!.#.>) "
    local dirs='[%F{yellow}%3(v|%32<..<%3v%<<|%60<..<%~%<<)%f]'
    local vcs='%3(v|[%25<\<<%F{yellow}%2v%f@%F{blue}%1v%f%<<]|)'
    RPROMPT="$dirs$vcs"
else
    PROMPT='%B%{${fg[red]}%}[%n%{${fg[blue]}%}@%m${WINDOW:+":$WINDOW"}]%{%(?.$fg[blue].$fg[red])%}%(!.#.$)%{${reset_color}%}%b '
    RPROMPT='%{${fg[green]}%}[%(5~,%-1~/.../%2~,%~)] %{${fg[magenta]}%}%B%T%b%{${reset_color}%} $(parse_git_branch)'
    SPROMPT="%B%r is correct? [n,y,a,e]:%b "
fi

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

#-----------------------------------------------
#  Utility function
#-----------------------------------------------
#
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

if [ x$TERM = xscreen ]; then
    alias ssh=ssh_screen
fi

# grep process
function psg() {
  psa | head -n 1 # show label
  psa | grep $* | grep -v "ps -auxww" | grep -v grep
}

# http://subtech.g.hatena.ne.jp/secondlife/20080604/1212562182
function cdf () {
    local -a tmpparent; tmpparent=""
    local -a filename; filename="${1}"
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

#------------------------
# MySQL
#------------------------
export MYSQL_PS1='(^[[32m\u^[[00m@^[[33m\h^[[00m) ^[[34m[\d]^[[00m > '

#------------------------
# Perl
#------------------------
# set PERL5LIB env
perl5lib () {
    export PERL5LIB="$PWD/lib:$PWD/t/lib:$PWD/t/*/lib:/opt/local/lib/perl5/site_perl/5.8.8/darwin-2level:${PERL5LIB}"
}

function cdmake () {
    cdf "Makefile.PL"
}

#------------------------
# ruby
#------------------------
refe_utf8() {
  refe $@ | nkf -Ew
}
alias refe='refe_utf8'

function cdrake () {
    cdf "Rakefile"
}


#-----------------------------------------------
#  completion
#-----------------------------------------------
# auto-fu
#bindkey -N isearch
#source ~/.zsh/auto-fu.zsh
#
#function zle-line-init () {
#    auto-fu-init;
#}
#zle -N zle-line-init

#-----------------------------------------------
# java settings
#-----------------------------------------------
[ -e ~/.zshrc-java ] && source ~/.zshrc-java

#-----------------------------------------------
# load mac config
#-----------------------------------------------
# mac
if [[ "${OSTYPE}" = darwin* ]] ; then
    [ -e ~/.zshrc-mac ] && source ~/.zshrc-mac
fi

#-----------------------------------------------
# load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
