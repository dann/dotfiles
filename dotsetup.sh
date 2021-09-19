#!/bin/bash

DOTFILES=(
.ackrc
.bash
.bashrc
.bashrc_profile
.ctags
.gdbinit
.gitconfig
.irbrc
.my.cnf
.pryrc
.pylint
.screenrc
.shipit
.tidyrc
.tmux
.tmux.conf
.vim
.vimrc
.zsh
.zshrc
)

setup() {
    remove_dotfiles
    link_dotfiles
    link_nvim_dotfiles
    setup_local_dir
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

link_nvim_dotfiles() {
    CURRENT_DIR=`pwd`
    ln -s $CURRENT_DIR/.config/nvim $HOME/.config/nvim
    ln -s $CURRENT_DIR/.config/fish $HOME/.config/fish
}

setup_local_dir() {
    mkdir $HOME/local
}

setup
