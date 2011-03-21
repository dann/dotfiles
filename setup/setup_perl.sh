#!/bin/sh
curl -LO http://xrl.us/perlbrew
perl perlbrew init
perl perlbrew install
perl perlbrew install -v perl-5.12.2
rm perlbrew
