#-----------------------------------------------
# vi-keys
#-----------------------------------------------
bindkey -v
bindkey "^P" up-line-or-history
bindkey "^N" down-line-or-history
bindkey '^F' beginning-of-line
bindkey '^E' end-of-line
bindkey "^W" forward-word
bindkey "^B" backward-word
bindkey '^H' backward-delete-char
bindkey "^K" vi-change-eol
bindkey "^L" push-line-or-edit

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
# zaw
#-----------------------------------------------
bindkey '^@' zaw-cdr
bindkey '^R' zaw-history
bindkey '^X^F' zaw-git-files
bindkey '^X^B' zaw-git-branches
bindkey '^X^P' zaw-process
bindkey '^A' zaw-tmux


# --------------
# anyframe
# --------------
zstyle ":anyframe:selector:" use peco
bindkey '^Z' anyframe-widget-cdr
bindkey '^R' anyframe-widget-put-history

