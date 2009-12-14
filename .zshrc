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

#-----------------------------------------------
#  alias
#-----------------------------------------------
alias df='df -h'
alias du='du -h'
alias j='jobs -l'
alias sl='ls'
alias gd='dirs -v; echo -n "select number: "; read newdir; cd -"$newdir"'
alias gf=grep-find
alias vi='vim'
alias cls='clear'
alias vip='vim **/*.{pm,css,tt2,t,js}'
alias ls='ls --color'
alias psa='ps auxw'

# ssh-agent
alias ssh-add-sh='eval `ssh-agent -s` ; ssh-add'

# global alias
alias -g V="| col -b | vim -R -"

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
# screen
#-----------------------------------------------
# show command name on screen's status bar
if [ "$TERM" = "screen" ]; then
	#chpwd () { echo -n "_`dirs`\\" }
	chpwd () { 
            if [[ "${OSTYPE}" = darwin* ]] ; then
                gls -al --color 
            else
                ls -al
            fi
        }
	preexec() {
		# see [zsh-workers:13180]
		# http://www.zsh.org/mla/workers/2000/msg03993.html
		emulate -L zsh
		local -a cmd; cmd=(${(z)2})
		case $cmd[1] in
			fg)
				if (( $#cmd == 1 )); then
					cmd=(builtin jobs -l %+)
				else
					cmd=(builtin jobs -l $cmd[2])
				fi
				;;
			%*) 
				cmd=(builtin jobs -l $cmd[1])
				;;
			cd)
				if (( $#cmd == 2)); then
					cmd[1]=$cmd[2]
				fi
				;&
			*)
				echo -n "k$cmd[1]:t\\"
				return
				;;
		esac

		local -A jt; jt=(${(kv)jobtexts})

		$cmd >>(read num rest
			cmd=(${(z)${(e):-\$jt$num}})
			echo -n "^[k$cmd[1]:t^[\\") 2>/dev/null
	}
	chpwd
fi

#-----------------------------------------------
# Prompot
#-----------------------------------------------
setopt prompt_subst

PROMPT='%B%{${fg[red]}%}[%n%{${fg[blue]}%}@%m${WINDOW:+":$WINDOW"}]%{%(?.$fg[blue].$fg[red])%}%(!.#.$)%{${reset_color}%}%b '
RPROMPT='%{${fg[green]}%}[%(5~,%-1~/.../%2~,%~)] %{${fg[magenta]}%}%B%T%b%{${reset_color}%} $(parse_git_branch)'
SPROMPT="%B%r is correct? [n,y,a,e]:%b "

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

#-----------------------------------------------
#  Utilit function
#-----------------------------------------------
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


# Perl
# ------------------------
# set PERL5LIB env
perl5lib () {
    export PERL5LIB="$PWD/lib:$PWD/t/lib:$PWD/t/*/lib:/opt/local/lib/perl5/site_perl/5.8.8/darwin-2level:${PERL5LIB}"
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

function cdmake () {
    cdf "Makefile.PL"
}

# alias for catalyst development (perl)
alias cs="perl script/*_server.pl -d"
alias carpcs="perl -MCarp::Always script/*_server.pl -d"

# ruby
# ------------------------
refe_utf8() {
  refe $@ | nkf -Ew
}
alias refe='refe_utf8'

function cdrake () {
    cdf "Rakefile"
}

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
