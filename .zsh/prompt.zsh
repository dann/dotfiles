#-----------------------------------------------
# Prompot
#-----------------------------------------------
setopt prompt_subst

if [[ $ZSH_VERSION == (<5->|4.<4->|4.3.<10->)* ]]; then
    autoload -Uz vcs_info
    zstyle ':vcs_info:(git|svn):*' formats '%R' '%S' '%b'
    zstyle ':vcs_info:(git|svn):*' actionformats '%R' '%S' '%b|%a'
    zstyle ':vcs_info:*' formats '%R' '%S' '%s:%b'
    zstyle ':vcs_info:*' actionformats '%R' '%S' '%s:%b|%a'
    precmd_vcs_info () {
        psvar=()
        LANG=en_US.UTF-8 vcs_info
        repos=`print -nD "$vcs_info_msg_0_"`
        [[ -n "$repos" ]] && psvar[2]="$repos"
        [[ -n "$vcs_info_msg_1_" ]] && psvar[3]="$vcs_info_msg_1_"
        [[ -n "$vcs_info_msg_2_" ]] && psvar[1]="$vcs_info_msg_2_"
    }
    typeset -ga precmd_functions
    precmd_functions+=precmd_vcs_info

    PROMPT="%(!.%F{red}.%F{green})%U%n@%6>>%m%>>%u%f:%1(j.%j.)%(!.#.>) "
    local dirs='[%F{yellow}%3(v|%32<..<%3v%<<|%60<..<%~%<<)%f]'
    local vcs='%3(v|[%25<\<<%F{yellow}%2v%f@%F{blue}%1v%f%<<]|)'
    RPROMPT="$dirs$vcs"
else
    if which git >& /dev/null ; then
        PROMPT='%B%{${fg[red]}%}[%n%{${fg[blue]}%}@%m${WINDOW:+":$WINDOW"}]%{%(?.$fg[blue].$fg[red])%}%(!.#.$)%{${reset_color}%}%b '
        RPROMPT='%{${fg[green]}%}[%(5~,%-1~/.../%2~,%~)] %{${fg[magenta]}%}%B%T%b%{${reset_color}%} $(parse_git_branch)'
        SPROMPT="%B%r is correct? [n,y,a,e]:%b "
    else
        PROMPT='%B%{${fg[red]}%}[%n%{${fg[blue]}%}@%m${WINDOW:+":$WINDOW"}]%{%(?.$fg[blue].$fg[red])%}%(!.#.$)%{${reset_color}%}%b '
        RPROMPT='%{${fg[green]}%}[%(5~,%-1~/.../%2~,%~)] %{${fg[magenta]}%}%B%T%b%{${reset_color}%}'
        SPROMPT="%B%r is correct? [n,y,a,e]:%b "
    fi
fi

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}
