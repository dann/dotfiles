#!/bin/sh

#===================================
# Configs 
#===================================
HOMEBREW_DIR=${HOME}/Dropbox/homebrew

#===================================
# Main 
#===================================
setup() {
    download_homebrew ${HOMEBREW_DIR}
    link_dropbox_dir
}

download_homebrew() {
    local download_to=$1

    mkdir -p ${download_to}
    curl -Lsf http://github.com/mxcl/homebrew/tarball/master | tar xz --strip 1 -C ${download_to}
}

link_dropbox_dir() {
    rm $HOME/homebrew
    ln -s $HOME/Dropbox/homebrew $HOME/homebrew
}

setup
