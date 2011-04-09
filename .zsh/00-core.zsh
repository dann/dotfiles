# change default file mode
umask 022

#-----------------------------------------------
# Base setting
#-----------------------------------------------
export LANG=ja_JP.utf8
#export LANG=en_US.UTF-8
export EDITOR=vim
export VISUAL=vim
export COLOR=32
export SCREENCOLOR=g
export PERL_BADLANG=0
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
export LESS="-erX"

# save history
export HISTSIZE=100000 HISTFILE=~/.zsh_history SAVEHIST=100000
export WORDCHARS='*?[]~=&;!#$%^(){}<>'

# MySQL
export MYSQL_PS1='(^[[32m\u^[[00m@^[[33m\h^[[00m) ^[[34m[\d]^[[00m > '

# version detection
autoload -Uz is-at-least

#-----------------------------------------------
# Path
#-----------------------------------------------
export PATH="/bin:${PATH}"
export PATH="/usr/bin:${PATH}"
export PATH="/usr/games:${PATH}"
export PATH="/sbin:${PATH}"
export PATH="/usr/sbin:${PATH}"
export PATH="/var/lib/gems/1.8/bin/:${PATH}"
export PATH="/opt/local/bin:${PATH}"
export PATH="/opt/local/sbin:${PATH}"
export PATH="/usr/local/bin:${PATH}"
export PATH="$HOME/bin:${PATH}"
export PATH="$HOME/devbin:${PATH}"
export PATH="$HOME/local/bin:${PATH}"

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
    magic_equal_subst \
#    correct \
#    correct_all \


# Don't add some commands to history
zshaddhistory() {
    local line=${1%%$'\n'}
    local cmd=${line%% *}
    [[  ${cmd} != (l|l[sal])
        && ${cmd} != (cd)
        && ${cmd} != (rm)
        && ${cmd} != (m|man)
    ]]
}

#-----------------------------------------------
# vi-keys
#-----------------------------------------------
bindkey -v
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history
bindkey "^K" vi-change-eol

if is-at-least 4.3.10; then
    bindkey '^R' history-incremental-pattern-search-backward
    bindkey '^S' history-incremental-pattern-search-forward
else
    bindkey "^R" history-incremental-search-backward
    bindkey '^S' history-incremental-search-forward
fi

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

# don't complete remote file path
zstyle ':completion:*:complete:scp:*:files' command command -

autoload -U compinit; compinit
autoload colors; colors

#-----------------------------------------------
# screen
#-----------------------------------------------
# show command name on screen's status bar
if [ "$TERM" = "screen" ]; then
	#chpwd () { echo -n "_`dirs`\\" }
	chpwd () { 
            if [[ "${OSTYPE}" = darwin* ]] ; then
                ls -al --color 
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

function chpwd() {
    if [[ "${OSTYPE}" = darwin* ]] ; then
        ls -al --color 
    else
        ls -al
    fi
}

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

# disable Ctrl+S
stty stop undef

#------------------------
# Perl
#------------------------
# perlbrew
#-----------------------------------------------
[ -e ~/perl5/perlbrew/etc/bashrc ] && source ~/perl5/perlbrew/etc/bashrc

#------------------------
# Python
#------------------------

# pythonbrew
#-----------------------------------------------
[ -e ~/.pythonbrew/etc/bashrc ] && source ~/.pythonbrew/etc/bashrc

#------------------------
# ruby
#------------------------

# rvm for ruby
#-----------------------------------------------
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
