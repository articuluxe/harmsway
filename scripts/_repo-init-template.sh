#!/bin/bash
# -*- Mode: sh -*-
# _repo-init-template.sh --- init a src repo
# Copyright (C) 2016-2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, May  3, 2016
# Version: 1.0
# Modified Time-stamp: <2018-02-01 15:04:39 dan.harms>
# Modified by: Dan Harms
# Keywords: src repo

if [ $# -lt 2 ]; then
    echo "!!! Usage: repo-init.sh project version [target]"
    exit 1
fi

proj=$1
ver=$2
subdir=${3:-$proj-$ver}
echo "Using $proj version $ver"

_yesorno.sh "Clone $proj into $(pwd)/$subdir/$proj?"
[ $? == 1 ] && exit
echo -e

if [ ! -d "$subdir"/"$proj"/.git ]; then
    mkdir -p "$subdir"
    # git clone ssh://git@addr:7999/proj/proj.git $subdir/proj
fi

cd "$subdir" || ( echo "$subdir does not exist; exiting..." && exit 1 )
repo=~/config/repos/"$proj"/"$ver"

# install any shell scripts (except this one), .env files, or project files
echo -e
echo "Installing files..."
find "$repo" -maxdepth 1 -name \*.sh -print | grep -v "$proj"-init.sh | xargs -I '{}' cp -pf '{}' .
find "$repo" -maxdepth 1 -name \*.env -print0 | xargs -0I '{}' cp -pf '{}' .
find "$repo" -maxdepth 1 -name \*.proviso -print0 | xargs -0I '{}' cp -pf '{}' .
echo "Installing files done."
echo -e

if [ ! -d "$proj"/.git/hooks ]; then
    echo "! $proj repo missing; install first."
    exit 1
fi

# code ends here
