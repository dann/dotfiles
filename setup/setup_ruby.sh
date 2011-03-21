#!/bin/bash
bash < <( curl http://rvm.beginrescueend.com/releases/rvm-install-head )
$HOME/.rvm/bin/rvm install 1.9.2
$HOME/.rvm/bin/rvm 1.9.2 --default
