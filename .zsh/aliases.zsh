# short aliases
alias a='alias'
alias c='clear'
alias h='cd ~'
alias l='ls -F'
alias m='cd /var/tmp/making'
alias u='cd ..'
alias v='less'
alias j='jobs -l'

# default options 
alias df='df -h'
alias du='du -h'
alias vi='vim'
alias cls='clear'
alias ls='ls --color'
alias tmux="TERM=screen-256color-bce tmux"

# typo
alias sl='ls'
alias inmae='iname'

# time
alias dates='date "+%Y-%m-%d %T.%N"'
alias today="date '+%Y%m%d'"
alias today-="date '+%Y-%m-%d'"
alias now="date '+%Y%m%d_%H%M%S'"
alias now-="date '+%Y-%m-%d_%H%M%S'"

# ssh-agent
alias ssh-add-sh='eval `ssh-agent -s` ; ssh-add'

# global alias
alias -g V="| col -b | vim -R -"

# dstat
alias dstat-full='dstat -Tclmdrn'
alias dstat-mem='dstat -Tclm'
alias dstat-cpu='dstat -Tclr'
alias dstat-net='dstat -Tclnd'
alias dstat-disk='dstat -Tcldr'

# some useful aliases
alias fastscp='scp -c arcfour -o Compression=no'
alias pa='ps auxwwww'
alias pspgid='ps axwww -o "ppid pgid pid user fname args"'
alias iops='ps auxwwww -L|awk "\$10 ~ /(D|STAT)/{print}"'
alias ipsort='sort -nt. +0 -1 +1 -2 +2 -3 +3'
alias sortpw='sort -t: -k3n /etc/passwd'
alias sortgrp='sort -t: -k3n /etc/group'
alias eman='LC_ALL=C LANG=C man'
alias cert2text='openssl x509 -text -in '
alias priv2text='openssl rsa -text -in '
alias gd='dirs -v; echo -n "select number: "; read newdir; cd -"$newdir"'
alias gf=grep-find

# Mac apps
alias mou="open /Applications/Mou.app"

if [ -x ~/local/bin/screen -o ]; then
   alias screen=$HOME/local/bin/screen
fi

if [ -x /usr/bin/rlwrap -o ]; then
    alias sqlplus='/usr/bin/rlwrap -if $HOME/.zsh/misc/.oracle_keywords sqlplus' 
    alias gnuplot='rlwrap -a -c gnuplot'
fi

