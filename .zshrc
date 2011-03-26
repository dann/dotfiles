#-----------------------------------------------
# load dev env config
#-----------------------------------------------
# share screen session with users in .dev_users 
# if we load this config
[ -e ~/.devenvrc ] && source ~/.devenvrc

#-----------------------------------------------
# load user config
#-----------------------------------------------
[ -e ~/.zshrc-before ] && source ~/.zshrc-before

#-----------------------------------------------
# load main settings
#-----------------------------------------------
[ -e ~/.zshrc-main ] && source ~/.zshrc-main

#-----------------------------------------------
# java settings
#-----------------------------------------------
[ -e ~/.zshrc-java ] && source ~/.zshrc-java

#-----------------------------------------------
# load mac config
#-----------------------------------------------
# mac
if [[ "${OSTYPE}" = darwin* ]] ; then
    [ -e ~/.zshrc-mac ] && source ~/.zshrc-mac
fi

#-----------------------------------------------
# load user config
#-----------------------------------------------
# if you want to customize this config,
# you need to create .zshrc-after 
[ -e ~/.zshrc-after ] && source ~/.zshrc-after
