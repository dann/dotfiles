#!/bin/sh

#===================================
# Configs 
#===================================
REPOSITORIES=(
homebrew/versions
phinze/homebrew-cask
homebrew/binary
caskroom/versions
motemen/ghq
tcr/tcr
)

PACKAGES=(
bvi
brew-cask
coreutils
clang-format
gettext
unzip
ghq
git
git-flow
git-now
htop
jsl
jsonpp
jq
lha
libiconv
lv
lynx
maven31
mercurial
multitail
node
neon
ngrep
nkf
nmap
plenv
perl-build
pkg-config
sqlite
subversion
the_silver_searcher
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
#eclipse-ide
flux
google-chrome
growl-fork
#iterm2
#java
firefox
keyremap4macbook
pandoc
#vagrant
#virtualbox
#xtrafinder
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
    brew uninstall --force $package && install coreutils --default-names

    for package in ${PACKAGES[@]}
    do
        brew uninstall --force $package && brew install $package
    done
}

install_dmgs() {
    rm -rf /opt/homebrew-cask

    for dmg in ${DMGS[@]}
    do
        brew cask uninstall --force $package && brew cask install $dmg
    done
}

setup