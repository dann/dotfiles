#!/bin/sh
curl -LO http://xrl.us/perlbrew | perl - App::cpanminus
cpanm < ~/env/dotfiles/setup/perl-modules.txt
