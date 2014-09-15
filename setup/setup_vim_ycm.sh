#!/bin/bash

cd ~/.vim/bundle
git clone https://github.com/Valloric/YouCompleteMe.git
cd ~/.vim/bundle/YouCompleteMe
git submodule update --init --recursive
./install.sh


