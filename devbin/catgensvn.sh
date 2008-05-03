#!/bin/bash

CATALYST_CMD='/home/dann/devbin/catsetup.pl'
CATALYST_APP_NAME=$1
SVN_REPO_PATH=/var/svn/repos/$CATALYST_APP_NAME
SVN_TARGET=http://localhost/svn/$CATALYST_APP_NAME

if [ -z $1 ]; then
    echo "You need to give a catalyst project name."
    return 1
fi

if [ -f $1 -o -d $1 ]; then
    echo "$1 exist in current directry.\nYou should change directry."
    return 1
fi

sudo svnadmin create --fs-type fsfs $SVN_REPO_PATH
sudo chown -R www-data:www-data $SVN_REPO_PATH

mkdir $1
cd $1
mkdir trunk tags branches
svn import . $SVN_TARGET -m "initial import"
cd ..
rm -rf $1

$CATALYST_CMD $CATALYST_APP_NAME
if [ $? -ne 0 ]; then
    echo "commad  catastarter failed."
    return 1
fi

svn import $CATALYST_APP_NAME $SVN_TARGET/trunk/ -m "import"
rm -rf $CATALYST_APP_NAME
svn co $SVN_TARGET/trunk $CATALYST_APP_NAME
cd $CATALYST_APP_NAME
svn remove logs/*
svn ci -m 'rm logfile'
svn up
svn propset svn:ignore "*.log" logs/
svn ci -m 'add ignore ./log/*.log'
svn up
svn propset svn:ignore "*" tmp/cache
svn ci -m "add ignore tmp/"
svn up
#cd ../
#rm -rf $CATALYST_APP_NAME
#svk mirror $SVN_TARGET //mirror/$CATALYST_APP_NAME
#svk sync //mirror/$CATALYST_APP_NAME
#svk copy //mirror/$CATALYST_APP_NAME //$CATALYST_APP_NAME
#svk co //$CATALYST_APP_NAME $CATALYST_APP_NAME
