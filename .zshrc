#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load zsh config files
#-----------------------------------------------
for config_file ($HOME/.zsh/*.zsh) source $config_file

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

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
