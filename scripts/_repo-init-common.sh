#!/bin/bash
# -*- Mode: sh -*-
# _repo-init-common.sh --- init a src repo
# Copyright (C) 2016-2018, 2021  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, May  3, 2016
# Version: 1.0
# Modified Time-stamp: <2021-11-10 11:26:38 dharms>
# Modified by: Dan Harms
# Keywords: src repo

if [ $# -lt 3 ]; then
    echo "!!! Usage: repo-init.sh project version target <force>"
    exit 1
fi

proj=$1
ver=$2
subdir=$3
force=$4
recurse=true
repo=~/config/repos/"$proj"/"$ver"
# for display purposes
target="$(pwd)/$subdir"

if [ "$subdir" == "." ]; then
    recurse=false
    target="$(pwd)"
fi

if [ -z "$force" ]; then
    _yesorno.sh "Clone $proj version $ver into $target?"
    [ $? == 1 ] && exit 1
fi

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
find "$repo" -maxdepth 1 -name \*.sh -print | grep -v "$proj"-init.sh \
    | grep -v _repo-init.sh | xargs -I '{}' cp -pfv '{}' .
find "$repo" -maxdepth 1 -name \*.env -print0 | xargs -0I '{}' cp -pfv '{}' .
find "$repo" -maxdepth 1 -name \*.proviso -print0 | xargs -0I '{}' cp -pfv '{}' .
find "$repo" -maxdepth 1 -name \*.tar -print0 | xargs -0I '{}' cp -pfv '{}' .
echo "Installing files done."
echo -e

# code ends here
