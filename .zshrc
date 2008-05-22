# change default file mode
umask 022

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
# Base setting
#-----------------------------------------------
export LANG=ja_JP.utf8
export EDITOR=vim
export VISUAL=vim
export COLOR=32
export SCREENCOLOR=g
export PERL_BADLANG=0
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'

# save history
export HISTSIZE=100000 HISTFILE=~/.zsh_history SAVEHIST=100000

#-----------------------------------------------
# option
#-----------------------------------------------
setopt \
    append_history \
    auto_cd \
    auto_list \
    auto_resume \
    cdable_vars \
    NO_clobber \
    complete_in_word \
    equals \
    extended_glob \
    extended_history \
    NO_glob_dots \
    hist_ignore_dups \
    NO_hist_ignore_space \
    NO_ignore_eof \
    interactive_comments \
    list_types \
    long_list_jobs \
    mail_warning \
    no_bad_pattern \
    notify \
    numeric_glob_sort \
    print_exit_value \
    pushd_minus \
    pushd_silent \
    pushd_to_home \
    pushd_ignore_dups \
    rc_quotes \
    share_history \
    autopushd \
    no_beep \
    auto_param_slash \

#    correct \
#    correct_all \

#-----------------------------------------------
# Path
#-----------------------------------------------
export PATH="/opt/local/bin:${PATH}"
export PATH="/opt/local/sbin:${PATH}"
export PATH="$HOME/bin:${PATH}"
export PATH="$HOME/devbin:${PATH}"
export PATH="/bin:${PATH}"
export PATH="/usr/local/bin:${PATH}"
export PATH="/usr/bin:${PATH}"
export PATH="/usr/games:${PATH}"
export PATH="/sbin:${PATH}"
export PATH="/usr/sbin:${PATH}"
export PATH="/var/lib/gems/1.8/bin/:${PATH}";

#-----------------------------------------------
# vi-keys
#-----------------------------------------------
bindkey -v
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history
bindkey "^K" vi-change-eol
bindkey "^R" history-incremental-search-backward

#-----------------------------------------------
# These works in linux+xterm+ssh setup:
#-----------------------------------------------
bindkey "^[[1~" vi-beginning-of-line   # Home
bindkey "^[[4~" vi-end-of-line         # End
bindkey "^[[2~" beep                   # Insert
bindkey "^[[3~" delete-char            # Del
bindkey "^[[A" up-line-or-history      # Up
bindkey "^[[B" down-line-or-history    # Down

#-----------------------------------------------
# completion
#-----------------------------------------------
fpath=(~/.zsh/completion $fpath)
_cache_hosts=(`perl -ne  'if (/^([a-zA-Z0-9.-]+)/) { print "$1\n";}' ~/.ssh/known_hosts`)
autoload -U compinit; compinit


#-----------------------------------------------
# command line
#-----------------------------------------------
autoload edit-command-line; zle -N edit-command-line
# <Esc>v to use vim to edit a command:
bindkey -M vicmd v edit-command-line

#-----------------------------------------------
# url escape
#-----------------------------------------------
autoload -U url-quote-magic
zle -N self-insert url-quote-magic

#-----------------------------------------------
# exclude list
#-----------------------------------------------
compctl -/ -g '*~(CVS|.svn)(/)' cd
compctl -g \
    '(.*|*~*.class~*.o~*.a~*.tar~*.gz~*.Z~*.rpm~*.gif~*.jpg~*.png~*.zip~*.jar)~.svn'\
    vim

fignore=(.o \~ .swp CVS)
zmodload zsh/complist

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

# ssh-agent
alias ssh-add-sh='eval `ssh-agent -s` ; ssh-add'

# global alias
alias -g V="| vim -R -"

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

PROMPT='%(?..exit %?)
 %{[33m%}%~%{[m%} %{[91m%}`$HOME/devbin/repospath.pl $(pwd)`%{[m%}%{[38m%}%(!.#.$)%{[m%}%{m%} '
RPROMPT='%{[38m%}[%n@%m]%{m%}%{[00m%}'

#-----------------------------------------------
#  Utilit function
#-----------------------------------------------
grep-find () { find . -type f -print0 | xargs -0 -e grep -n --binary-files=without-match -e $@ | grep -E -v \(\*.\*~\|tags\) }

# show all history
function history-all { history -E 1 }

# set PERL5LIB env
perl5lib () {
    export PERL5LIB="$PWD/lib:$PWD/t/lib:${PERL5LIB}"
}

function ssh_screen(){
    A=$#
    eval server=$"$A"
    screen -t $server ssh "$@"
}

if [ x$TERM = xscreen ]; then
    alias ssh=ssh_screen
fi

#-----------------------------------------------
# load mac config
#-----------------------------------------------
# mac
if [[ "${OSTYPE}" = darwin* ]] ; then
    [ -e ~/.zshrc-mac ] && source ~/.zshrc-mac
fi

#[ -e ~/.zshrc.prompt ] && source ~/.zshrc.prompt

#-----------------------------------------------
# load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
