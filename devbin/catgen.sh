#!/bin/bash
CATALYST_CMD='/home/dann/devbin/catstarter.pl'
APP_NAME=$1
APP_PREFIX=`echo $APP_NAME | tr "[A-Z]" "[a-z]"`
GIT_REPOS=dann@192.168.0.30:/var/git/repos

if [ -z $1 ]; then
    echo "You need to give a catalyst project name."
    return 1
fi

if [ -f $1 -o -d $1 ]; then
    echo "$1 exist in current directry.\nYou should change directry."
    return 1
fi

$CATALYST_CMD $APP_NAME
cd $APP_NAME
git init-db
git add .
git commit -a -m "initial commit"
cd ..
echo "commited $APP_NAME to local git repository"
git clone --bare $APP_NAME/.git $APP_PREFIX.git
echo "cloned $APP_NAME"
scp -r $APP_PREFIX.git $GIT_REPOS
echo "copied $APP_NAME to remote git repository"
rm -rf $APP_PREFIX.git
cd $APP_NAME
git remote add origin $GIT_REPOS/$APP_PREFIX.git
echo "registered $GIT_REPOS/$APP_PREFIX.git as remote git repository"
git fetch origin
echo "fetched origin done!"
