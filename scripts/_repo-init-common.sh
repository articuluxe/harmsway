#!/bin/bash
# -*- Mode: sh -*-
# _repo-init-common.sh --- init a src repo
# Copyright (C) 2016-2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, May  3, 2016
# Version: 1.0
# Modified Time-stamp: <2018-02-02 10:40:45 dan.harms>
# Modified by: Dan Harms
# Keywords: src repo

if [ $# -lt 3 ]; then
    echo "!!! Usage: repo-init.sh project version target"
    exit 1
fi

proj=$1
ver=$2
subdir=$3
recurse=true
repo=~/config/repos/"$proj"/"$ver"
# for display purposes
target="$(pwd)/$subdir"

if [ "$subdir" == "." ]; then
    recurse=false
    target="$(pwd)"
fi

echo "Using $proj version $ver"
_yesorno.sh "Clone $proj into $target?"
[ $? == 1 ] && exit 1
echo -e

if [ "$recurse" = true ]; then
    mkdir -p "$subdir"
    cd "$subdir" || ( echo "$subdir does not exist; exiting..." && exit 1 )
fi

[ -r "$repo"/clone ] && "$repo"/clone

if [ "$recurse" = false ]; then
    cd "$proj" || ( echo "$proj does not exist; exiting..." && exit 1 )
fi

# install any shell scripts (except this one), .env files, or project files
echo -e
echo "Installing files from $repo..."
find "$repo" -maxdepth 1 -name \*.sh -print | grep -v "$proj"-init.sh | xargs -I '{}' cp -pfv '{}' .
find "$repo" -maxdepth 1 -name \*.env -print0 | xargs -0I '{}' cp -pfv '{}' .
find "$repo" -maxdepth 1 -name \*.proviso -print0 | xargs -0I '{}' cp -pfv '{}' .
echo "Installing files done."
echo -e

# code ends here
