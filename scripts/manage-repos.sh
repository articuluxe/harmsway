#!/bin/bash
# -*- Mode: sh -*-
# manage-repos.sh --- clone and update repos
# Copyright (C) 2017, 2019-2020  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 17, 2017
# Version: 1.0
# Modified Time-stamp: <2020-07-24 08:50:03 dharms>
# Modified by: Dan Harms
# Keywords: git repo

dir=${1:-~/src/harmsway/repos}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1

find "$dir" -type f -name clone | sort | while read -r fname; do
    parent=$( dirname "$fname" )
    cd "$parent" || exit
    repo=$( basename "$PWD" )
    echo -e
    echo "-*- $repo -*-"
    # clone
    if [ ! -d "$parent/src" ] && [ -f "$parent/clone" ]; then
        echo "Cloning $repo"
        ./clone
    fi
    # update
    if [ -d "$parent/src/.git" ]; then
        cd src || exit
        git fetch --all
        res=$( git log HEAD..origin --oneline )
        if [ -n "$res" ]; then
            echo -e
            echo "--- Updating $repo due to the following differences:"
            echo "$res"
            echo -e
            git pull
        fi
        cd "$parent" || exit
    elif [ -d "$parent/src/.hg" ]; then
        cd src || exit
        hg pull
        hg update
        cd "$parent" || exit
    fi
    # install
    if [ -f "$parent/.ignore_install" ]; then
        :
    elif [ -f "$parent/install_$(uname -s)" ]; then
        ./"install_$(uname -s)"
    elif [ -f "$parent/install" ]; then
        ./install
    fi
done

# code ends here
