if [ x$TERM = xscreen ]; then
    alias ssh=ssh_screen
fi

if [ -x /usr/bin/rlwrap -o ]; then
    alias sqlplus='/usr/bin/rlwrap -if $HOME/.zsh/.oracle_keywords sqlplus' 
fi

alias df='df -h'
alias du='du -h'
alias j='jobs -l'
alias sl='ls'
alias gd='dirs -v; echo -n "select number: "; read newdir; cd -"$newdir"'
alias gf=grep-find
alias vi='vim'
alias cls='clear'
alias ls='ls --color'

if [ -x ~/local/bin/screen -o ]; then
   alias screen=$HOME/local/bin/screen
fi

# ssh-agent
alias ssh-add-sh='eval `ssh-agent -s` ; ssh-add'

# global alias
alias -g V="| col -b | vim -R -"


