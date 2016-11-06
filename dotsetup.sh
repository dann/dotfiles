#!/bin/bash

DOTFILES=(
.Xdefaults
.ackrc
.bash
.bashrc
.bashrc_profile
.ctags
.dev_users
.devenvrc
.gdbinit
.gemrc
.gitconfig
.gvimrc
.irbrc
.my.cnf
.noserc
.perltidyrc
.pryrc
.pylint
.screenrc
.shipit
.tidyrc
.tmux
.tmux.conf
.tscreenrc
.vim
.vimrc
.vrapperrc
.zsh
.zshrc
devbin
devtools
)

setup() {
    remove_dotfiles
    link_dotfiles
    install_vim_plugins
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

install_vim_plugins() {
    vim +GoInstallBinaries +qall
}

setup
