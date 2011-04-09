#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load zsh config files
#-----------------------------------------------
for config_file ($HOME/.zsh/*.zsh) source $config_file

#-----------------------------------------------
# Load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
