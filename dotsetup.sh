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
.zsh
.zshrc
devbin
devtools
)

# Remove dotfiles
for dotfile in ${DOTFILES[@]}
do
    rm $HOME/$dotfile
done

# Link dotfiles
CURRENT_DIR=`pwd`
for dotfile in ${DOTFILES[@]}
do
    ln -s $CURRENT_DIR/$dotfile $HOME/$dotfile
done

chmod 700 ~/devbin/*
