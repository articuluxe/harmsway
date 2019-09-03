#!/bin/bash
# -*- Mode: sh -*-
# manage-repos.sh --- clone and update repos
# Copyright (C) 2017, 2019  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 17, 2017
# Version: 1.0
# Modified Time-stamp: <2019-09-03 11:05:09 dan.harms>
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
    if [ -d "$parent/src/.git" ]; then
        pushd src &> /dev/null
        git fetch --all
        res=$( git log HEAD..origin --oneline )
        if [ -n "$res" ]; then
            echo -e
            echo "--- Updating $repo due to the following differences:"
            echo "$res"
            echo -e
            git pull
        fi
        popd &> /dev/null
    elif [ -d "$parent/src/.hg" ]; then
        pushd src &> /dev/null
        hg pull
        hg update
        popd &> /dev/null
    fi
    # install
    if [ -f "$parent/.ignore_install" ]; then
        :
    elif [ -f "$parent/install_$(uname -s)" ]; then
        ./"install_$(uname -s)"
    elif [ -f "$parent/install" ]; then
        ./install
    fi
    popd &> /dev/null
done

# code ends here
