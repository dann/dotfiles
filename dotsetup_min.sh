#!/bin/bash

rm ~/.zshrc ~/.screenrc ~/.vimrc ~/.Xdefaults  ~/.bashrc ~/.bash_profile ~/.bash

CURRENT_DIR=`pwd`
ln -s $CURRENT_DIR/.zsh/00-core.zsh ~/.zshrc
ln -s $CURRENT_DIR/.screenrc ~/.screenrc
ln -s $CURRENT_DIR/.vim/vimrc-minimum ~/.vimrc
ln -s $CURRENT_DIR/.bash ~/.bash
ln -s $CURRENT_DIR/.bashrc_profile ~/.bashrc_profile
ln -s $CURRENT_DIR/.bashrc ~/.bashrc

