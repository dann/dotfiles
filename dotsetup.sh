#!/bin/bash

rm ~/.zshrc ~/.zshrc-minimum ~/.screenrc ~/.vimrc ~/.irbrc ~/.ctags ~/.vim ~/devbin ~/.zsh ~/.my.cnf ~/.dev_users ~/.devenvrc ~/.vimrc-before ~/.tidyrc ~/.perltidyrc  ~/.gitconfig  ~/.zshrc-mac ~/.vimrc-before ~/.vimrc-mac ~/.vimrc-minimum ~/.vimrc-plugins ~/.Xdefaults ~/.zshrc-java ~/.noserc ~/.pylint ~/.bashrc ~/.bash_profile ~/.bashrc-minimum  ~/.bashrc-mac ~/.bash ~/.shipit ~/.tscreenrc
rm -rf ~/.asstarter

CURRENT_DIR=`pwd`
ln -s $CURRENT_DIR/.zshrc ~/.zshrc
ln -s $CURRENT_DIR/.zshrc-minimum ~/.zshrc-minimum
ln -s $CURRENT_DIR/.zsh ~/.zsh
ln -s $CURRENT_DIR/.zshrc-java ~/.zshrc-java
ln -s $CURRENT_DIR/.zshrc-mac ~/.zshrc-mac
ln -s $CURRENT_DIR/.screenrc ~/.screenrc
ln -s $CURRENT_DIR/.tscreenrc ~/.tscreenrc
ln -s $CURRENT_DIR/.vimrc ~/.vimrc
ln -s $CURRENT_DIR/.vim ~/.vim
ln -s $CURRENT_DIR/.vim/bundle_vim72 ~/.vim/bundle
ln -s $CURRENT_DIR/.vimrc-mac ~/.vimrc-mac
ln -s $CURRENT_DIR/.vimrc-minimum ~/.vimrc-minimum
ln -s $CURRENT_DIR/.vimrc-before ~/.vimrc-before
ln -s $CURRENT_DIR/.vimrc-plugins ~/.vimrc-plugins
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
ln -s $CURRENT_DIR/.noserc ~/.noserc
ln -s $CURRENT_DIR/.pylint ~/.pylint
ln -s $CURRENT_DIR/.zshrc-java ~/.zshrc-java
ln -s $CURRENT_DIR/.bash ~/.bash
ln -s $CURRENT_DIR/.bashrc_profile ~/.bashrc_profile
ln -s $CURRENT_DIR/.bashrc-minimum ~/.bashrc-minimum
ln -s $CURRENT_DIR/.bashrc ~/.bashrc
ln -s $CURRENT_DIR/.bashrc-mac ~/.bashrc-mac
ln -s $CURRENT_DIR/.shipit ~/.shipit

chmod 700 ~/devbin/*
