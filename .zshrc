#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load zsh config files
#-----------------------------------------------
for config_file ($HOME/.zsh/*.zsh) source $config_file

#-----------------------------------------------
# Load settings for each hosts
#-----------------------------------------------
h="${HOST%%.*}"
if [[ -f "$HOME/.zsh/host-$h.zshrc" ]]; then
    source "$HOME/.zsh/host-$h.zshrc"
fi

#-----------------------------------------------
# Load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
