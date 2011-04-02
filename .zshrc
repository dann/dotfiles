#-----------------------------------------------
# Load dev env config
#-----------------------------------------------
# share screen session with users in .dev_users 
# if we load this config
[ -e ~/.devenvrc ] && source ~/.devenvrc

#-----------------------------------------------
# Load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# Load main settings
#-----------------------------------------------
[ -e ~/.zshrc-main ] && source ~/.zshrc-main

#-----------------------------------------------
# java settings
#-----------------------------------------------
[ -e ~/.zshrc-java ] && source ~/.zshrc-java

#-----------------------------------------------
# Load mac config
#-----------------------------------------------
# mac
if [[ "${OSTYPE}" = darwin* ]] ; then
    [ -e ~/.zshrc-mac ] && source ~/.zshrc-mac
fi

#-----------------------------------------------
# Load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
