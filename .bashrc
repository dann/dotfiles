#-------------------------------------------------------------
# load custom config
#-------------------------------------------------------------
[ -r ~/.bashrc_before ] && source ~/.bashrc_before

#-------------------------------------------------------------
# load minimum settings
#-------------------------------------------------------------
source ~/.bashrc_minimum

#-------------------------------------------------------------
# basics
#-------------------------------------------------------------
export LANG=ja_JP.UTF-8
export TERM=xterm-color

#-------------------------------------------------------------
# Prompt 
#-------------------------------------------------------------
color_prompt=yes
if [ "$color_prompt" = yes ]; then
    GREEN="\033[01;32m"; blue="\033[00;34m"
    cyan="\033[00;36m"; none="\033[00m"
    if [ -x /usr/bin/tput ] && tput setaf 1 >& /dev/null; then
        if [ $TERM = screen ];then
            PS1="\[$GREEN\]\u@\h\[$none\]:\[$blue\]\w\[$cyan\]\$\[$none\] "
            PS2="\[$cyan\]>\[$none\] "
        else
            PS1="\[$GREEN\]\u@\h\[$none\]:\[$blue\]\w\[$none\]\$ "
            PS2="> "
        fi
    fi
else
    PS1="\u@\h:\w\$ "
fi
unset color_prompt GREEN blue cyan none

#-------------------------------------------------------------
# completion
#-------------------------------------------------------------
if [ -f ~/.bash/bash_completion ]; then
    export BASH_COMPLETION="$HOME/.bash/bash_completion"
    source $BASH_COMPLETION
fi

#-------------------------------------------------------------
# load custom config
#-------------------------------------------------------------

[ -r ~/.bashrc_after ] && source ~/.bashrc_after
