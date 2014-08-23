
# peco hitory
function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(history -n 1 | \
        eval $tac | \
        peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    zle clear-screen
}
zle -N peco-select-history


function peco-global() {
    global $@ | peco | xargs less
}

function peco-pid() {
    ps ax | peco | awk '{ print $1 }'
}

function peco-cdr () {
    local selected_dir=$(cdr -l | awk '{ print $2 }' | peco)
    if [ -n "$selected_dir" ]; then
        BUFFER="cd ${selected_dir}"
        zle accept-line
    fi
    zle clear-screen
}
zle -N peco-cdr

function agvim () {
    local searcher
    if which ag > /dev/null; then
        searcher="ag"
    else
        searcher="ack"
    fi
    vim $($searcher $@ | peco --query "$LBUFFER" | awk -F : '{print "-c " $2 " " $1}')
}

function peco-ag() {
    local searcher
    if which ag > /dev/null; then
        searcher="ag"
    else
        searcher="ack"
    fi
    $searcher $@ | peco --query "$LBUFFER" | awk -F : '{print "-c " $2 " " $1}'
}
