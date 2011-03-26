#!/bin/sh
GEMSET=${1:-"default"}
rvm gemset create $GEMSET
rvm gemset use $GEMSET
bundle install
