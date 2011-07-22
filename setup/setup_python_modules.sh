#!/bin/bash

sudo easy_install pip

MODULES=(
virtualenvwrapper
pylint
pyflakes
pep8
nose
)

setup() {
    install_modules 
}

install_modules() {
    for module in ${MODULES[@]}
    do
        sudo pip install $module
    done
}

setup
