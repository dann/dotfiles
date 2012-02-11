#!/bin/bash

DOTFILES=(
.Xdefaults
.bash
.bashrc
.bashrc_profile
.ctags
.dev_users
.devenvrc
.gdbinit
.gitconfig
.irbrc
.irssi
.my.cnf
.noserc
.perltidyrc
.pylint
.screenrc
.shipit
.tidyrc
.tscreenrc
.vim
.vimrc
.vrapperrc
.zsh
.zshrc
devbin
devtools
.tmux.conf
)

setup() {
    remove_dotfiles
    link_dotfiles
    make_executable
}

remove_dotfiles() {
    for dotfile in ${DOTFILES[@]}
    do
        rm $HOME/$dotfile
    done
}

link_dotfiles() {
    CURRENT_DIR=`pwd`
    for dotfile in ${DOTFILES[@]}
    do
        ln -s $CURRENT_DIR/$dotfile $HOME/$dotfile
    done
}

make_executable() {
    chmod 700 ~/devbin/*
}

setup
