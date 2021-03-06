umask 022
ulimit -c 0
shopt -u sourcepath

if [[ "$PS1" ]]; then
    # Interactive mode

    #-------------------------------------------------------------
    # Shell options
    #-------------------------------------------------------------
    IGNOREEOF=10
    unset MAIL
    unset command_not_found_handle
    shopt -s histappend
    shopt -s histverify
    shopt -s histreedit
    shopt -s checkwinsize
    shopt -u hostcomplete
    shopt -s checkhash
    shopt -s no_empty_cmd_completion

    #-------------------------------------------------------------
    # Env vars
    #-------------------------------------------------------------
    unset LANG
    export PAGER=less
    export EDITOR=vi
    export LESS='-X -i -P ?f%f:(stdin).  ?lb%lb?L/%L..  [?eEOF:?pb%pb\%..]'
    export RSYNC_RSH=ssh
    export CVS_RSH=ssh

    # Terminal setting
    #eval `SHELL=sh tset -sQI`
    stty sane erase ^? intr ^C eof ^D susp ^Z quit ^\\ start ^- stop ^-

    #-------------------------------------------------------------
    # Prompt setting
    #-------------------------------------------------------------
    export PS1="[\u@\h:\w]\$ "

    #-------------------------------------------------------------
    # Completion
    #-------------------------------------------------------------
    complete -d cd
    complete -c man
    complete -v unset

    #-------------------------------------------------------------
    # history
    #-------------------------------------------------------------
    function share_history {
      history -a
      history -c
      history -r
    }
    PROMPT_COMMAND='share_history'
    export HISTSIZE=300000
    export HISTFILESIZE=300000
    export HISTCONTROL=ignoreboth
    export HISTFILE=~/.bash_myhistory

    function fceditor ()  {  tt=/tmp/$$.sh; cat $1 | ( read -rd '' s; t=/tmp/$$; echo "function a(){" > $t; echo "$s" >> $t; echo -e " }\n declare -f a" >> $t; chmod +x $t; $t > /dev/null 2>&1; if [ $? = 0 ]; then a=`$t|sed '1,2d;$d;s/....//'`";"`echo "$s"|tail -1|sed 's/^.\+\(#[^#]\+\)$/\1/'`; else a="$s"; fi; rm $t; echo "$a" ) > $tt; vim $tt; sed 's/^ *//' $tt > $1; rm $tt; }
    export FCEDIT=fceditor

fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
