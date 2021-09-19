#-----------------------------------------------
# Base setting
#-----------------------------------------------
export LANG=ja_JP.utf8
export EDITOR=vim
export VISUAL=vim
export COLOR=32
export SCREENCOLOR=g
export PERL_BADLANG=0
export LESS="-erX"

# save history
export HISTSIZE=100000 HISTFILE=~/.zsh_history SAVEHIST=100000
export WORDCHARS='*?[]~=&;!#$%^(){}<>'

# MySQL
export MYSQL_PS1='(^[[32m\u^[[00m@^[[33m\h^[[00m) ^[[34m[\d]^[[00m > '


#-----------------------------------------------
# Path
#-----------------------------------------------
export PATH="/bin:${PATH}"
export PATH="/usr/bin:${PATH}"
export PATH="/usr/games:${PATH}"
export PATH="/sbin:${PATH}"
export PATH="/usr/sbin:${PATH}"
export PATH="/opt/local/bin:${PATH}"
export PATH="/opt/local/sbin:${PATH}"
export PATH="/usr/local/bin:${PATH}"
export PATH="/usr/local/sbin:${PATH}"
export PATH="${HOME}/bin:${PATH}"
export PATH="${HOME}/devbin:${PATH}"
export PATH="${HOME}/local/bin:${PATH}"
export PATH="${HOME}/.local/bin:${PATH}"
export PATH="${HOME}/mysql-build/bin:${PATH}"

# Go
export GOPATH=$HOME/go
export PATH="${PATH}:${GOROOT}/bin:${GOPATH}/bin"


