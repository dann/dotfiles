if [ -x /usr/bin/rlwrap -o ]; then
    alias sqlplus='/usr/bin/rlwrap -if $HOME/.zsh/misc/.oracle_keywords sqlplus' 
    alias gnuplot='rlwrap -a -c gnuplot'
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

alias tmux="TERM=screen-256color-bce tmux"

if [ -x ~/local/bin/screen -o ]; then
   alias screen=$HOME/local/bin/screen
fi

# ssh-agent
alias ssh-add-sh='eval `ssh-agent -s` ; ssh-add'

# global alias
alias -g V="| col -b | vim -R -"


