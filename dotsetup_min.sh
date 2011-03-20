#!/bin/bash

rm ~/.zshrc ~/.zshrc-minimum ~/.screenrc ~/.vimrc  ~/.vimrc-minimum ~/.Xdefaults  ~/.bashrc ~/.bash_profile ~/.bashrc-minimum ~/.bash

CURRENT_DIR=`pwd`
ln -s $CURRENT_DIR/.zshrc-minimum ~/.zshrc
ln -s $CURRENT_DIR/.screenrc ~/.screenrc
ln -s $CURRENT_DIR/.vimrc-minimum ~/.vimrc
ln -s $CURRENT_DIR/devbin/.asstarter ~/.asstarter
ln -s $CURRENT_DIR/.bash ~/.bash
ln -s $CURRENT_DIR/.bashrc_profile ~/.bashrc_profile
ln -s $CURRENT_DIR/.bashrc-minimum ~/.bashrc

