#!/bin/sh

#===================================
# Configs 
#===================================
REPOSITORIES=(
homebrew/versions
phinze/homebrew-cask
homebrew/binary
)

PACKAGES=(
bvi
brew-cask
coreutils
gettext
git
git-flow
git-now
htop
jsl
lha
libiconv
lv
lynx
multitail
neon
ngrep
nkf
nmap
pkg-config
sqlite
subversion
tig
tmux
tree
valgrind
w3m
wget
zsh
)

DMGS=(
alfred
caffeine
dash
day-o
dropbox
eclipse-ide
flux
google-chrome
growl-fork
iterm2
java
firefox
keyremap4macbook
pandoc
vagrant
virtualbox
xtrafinder
)

#===================================
# Main 
#===================================
setup() {
    add_repositories
    install_packages
    install_dmgs
}

add_repositories() {
    for repository in ${REPOSITORIES[@]}
    do
        brew tap $repository
    done
}

install_packages() {
    brew install coreutils --default-names

    for package in ${PACKAGES[@]}
    do
        brew install $package
    done
}

install_dmgs() {
    for dmg in ${DMGS[@]}
    do
        brew cask install $dmg
    done
}

setup
