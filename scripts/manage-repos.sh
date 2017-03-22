#!/bin/bash
# -*- Mode: sh -*-
# manage-repos.sh --- clone and update repos
# Copyright (C) 2017  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 17, 2017
# Version: 1.0
# Modified Time-stamp: <2017-03-22 06:33:31 dharms>
# Modified by: Dan Harms
# Keywords: git repo

dir=${1:-~/src/harmsway/lisp}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir"

find "$dir" -type f -name clone | while read fname; do
    parent=$( dirname "$fname" )
    pushd "$parent" &> /dev/null
    repo=$( basename "$PWD" )
    echo -e
    echo "-*- $repo -*-"
    # clone
    if [ ! -d "$parent/src" -a -f "$parent/clone" ]; then
        echo "Cloning $repo"
        ./clone
    fi
    # update
    if [ -d "$parent/src" ]; then
        cd src
        git fetch --all
        res=$( git log HEAD.. --oneline )
        if [ -n "$res" ]; then
            echo -e
            echo "Updating $repo due to the following differences:"
            echo "$res"
            git pull
        fi
        cd ..
    fi
    # install
    if [ -f "$parent/install" ]; then
        ./install
    fi
    popd &> /dev/null
done

# code ends here
