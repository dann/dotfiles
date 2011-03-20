#!/bin/sh
CURRENT_DIR=`pwd`
curl -LO http://xrl.us/perlbrew | perl - App::cpanminus
cpanm < ${CURRENT_DIR}/setup/perl-modules.txt
