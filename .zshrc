#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load zsh config files
#-----------------------------------------------
source ~/.zplug/zplug
# for config_file ($HOME/.zsh/*.zsh) source $config_file
source ~/.zsh/zplug.zsh

if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    else
        echo
    fi
fi
zplug load --verbose

#-----------------------------------------------
# Load host specific settings
#-----------------------------------------------
h="${HOST%%.*}"
if [[ -f "$HOME/.zsh/host/$h.zsh" ]]; then
    source "$HOME/.zsh/host/$h.zsh"
fi

#-----------------------------------------------
# Load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
