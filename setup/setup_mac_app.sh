#!/bin/sh
CURRENT_DIR=`pwd`
brew install coreutils --default-names
brew install `cat ${CURRENT_DIR}/homebrew.txt`
