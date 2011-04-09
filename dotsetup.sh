#!/bin/bash

# Remove current settings 
rm ~/.Xdefaults 
rm ~/.bash 
rm ~/.bash_profile 
rm ~/.bashrc 
rm ~/.ctags 
rm ~/.dev_users 
rm ~/.devenvrc 
rm ~/.gdbinit
rm ~/.gitconfig  
rm ~/.irbrc 
rm ~/.my.cnf 
rm ~/.noserc 
rm ~/.perltidyrc  
rm ~/.pylint 
rm ~/.screenrc 
rm ~/.shipit 
rm ~/.tidyrc
rm ~/.tscreenrc 
rm ~/.vim 
rm ~/.vimrc 
rm ~/.zsh 
rm ~/.zshrc 
rm ~/devbin 

# Link dotfiles
CURRENT_DIR=`pwd`
ln -s $CURRENT_DIR/.Xdefaults       ~/.Xdefaults
ln -s $CURRENT_DIR/.bash            ~/.bash
ln -s $CURRENT_DIR/.bashrc          ~/.bashrc
ln -s $CURRENT_DIR/.bashrc_profile  ~/.bashrc_profile
ln -s $CURRENT_DIR/.ctags           ~/.ctags
ln -s $CURRENT_DIR/.dev_users       ~/.dev_users
ln -s $CURRENT_DIR/.devenvrc        ~/.devenvrc
ln -s $CURRENT_DIR/.gdbinit         ~/.gdbinit
ln -s $CURRENT_DIR/.gitconfig       ~/.gitconfig
ln -s $CURRENT_DIR/.irbrc           ~/.irbrc
ln -s $CURRENT_DIR/.irssi           ~/.irssi
ln -s $CURRENT_DIR/.my.cnf          ~/.my.cnf
ln -s $CURRENT_DIR/.noserc          ~/.noserc
ln -s $CURRENT_DIR/.perltidyrc      ~/.perltidyrc
ln -s $CURRENT_DIR/.pylint          ~/.pylint
ln -s $CURRENT_DIR/.screenrc        ~/.screenrc
ln -s $CURRENT_DIR/.shipit          ~/.shipit
ln -s $CURRENT_DIR/.tidyrc          ~/.tidyrc
ln -s $CURRENT_DIR/.tscreenrc       ~/.tscreenrc
ln -s $CURRENT_DIR/.vim             ~/.vim
ln -s $CURRENT_DIR/.vimrc           ~/.vimrc
ln -s $CURRENT_DIR/.zsh             ~/.zsh
ln -s $CURRENT_DIR/.zshrc           ~/.zshrc
ln -s $CURRENT_DIR/devbin           ~/devbin
ln -s $CURRENT_DIR/devtools         ~/devtools

chmod 700 ~/devbin/*
