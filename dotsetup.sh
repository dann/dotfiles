#!/bin/bash

rm ~/.zshrc ~/.screenrc ~/.vimrc ~/.irbrc ~/.ctags ~/.vim ~/devbin ~/.zsh ~/.my.cnf ~/.dev_users ~/.devenvrc ~/.vimrc-before ~/.tidyrc ~/.perltidyrc ~/.catsetup ~/.gitconfig ~/.catsetupr ~/.zshrc-mac ~/.vimrc-before ~/.vimrc-mac ~/.Xdefaults ~/.zshrc-java
rm -rf ~/.asstarter

CURRENT_DIR=`pwd`
ln -s $CURRENT_DIR/.zshrc ~/.zshrc
ln -s $CURRENT_DIR/.zsh ~/.zsh
ln -s $CURRENT_DIR/.zshrc-java ~/.zshrc-java
ln -s $CURRENT_DIR/.zshrc-mac ~/.zshrc-mac
ln -s $CURRENT_DIR/.screenrc ~/.screenrc
ln -s $CURRENT_DIR/.vimrc ~/.vimrc
ln -s $CURRENT_DIR/.vim ~/.vim
ln -s $CURRENT_DIR/.vimrc-mac ~/.vimrc-mac
ln -s $CURRENT_DIR/.vimrc-before ~/.vimrc-before
ln -s $CURRENT_DIR/.ctags ~/.ctags
ln -s $CURRENT_DIR/.irbrc ~/.irbrc
ln -s $CURRENT_DIR/devbin ~/devbin
ln -s $CURRENT_DIR/devtools ~/devtools
ln -s $CURRENT_DIR/.my.cnf ~/.my.cnf
ln -s $CURRENT_DIR/.dev_users ~/.dev_users
ln -s $CURRENT_DIR/.devenvrc ~/.devenvrc
ln -s $CURRENT_DIR/.tidyrc ~/.tidyrc
ln -s $CURRENT_DIR/.perltidyrc ~/.perltidyrc
ln -s $CURRENT_DIR/.gitconfig ~/.gitconfig
ln -s $CURRENT_DIR/.Xdefaults ~/.Xdefaults
ln -s $CURRENT_DIR/.irssi ~/.irssi
ln -s $CURRENT_DIR/devbin/.asstarter ~/.asstarter

chmod 700 ~/devbin/*
