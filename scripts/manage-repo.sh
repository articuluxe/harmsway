#!/bin/bash
# -*- Mode: sh -*-
# manage-repo.sh --- clone and update a repo
# Copyright (C) 2021, 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, November  5, 2021
# Version: 1.0
# Modified Time-stamp: <2023-01-30 13:41:09 dharms>
# Modified by: Dan Harms
# Keywords: git repo

parent=${1:-$(pwd)}

if [ ! -d "$parent" ]; then
    echo "!!! $parent is not a valid directory; exiting..."
    exit 1
fi

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
if [ -d "$parent/src/.git" ] \
       || [ -d "$parent/src/.git-ignore" ] \
       || [ -d "$parent/src/.git-ignore_$(uname -s)" ]; then
    if [ -f "update" ]; then
        updatefile="$parent/update"
    fi
    if [ -f "external" ]; then
        rel=$( cat external )
        abs=$( _realpath.sh "$rel" )
        if [ -d "$abs" ]; then
            echo "Changing path based on ./external to $abs"
            cd "$abs" || exit 1
        else
            echo "!!! $abs did not exist; ignoring ./external"
            cd "$parent/src" || exit 1
        fi
    else
        cd "$parent/src" || exit 1
    fi
    if [ -d "$parent/src/.git" ]; then
        # only run these commands for a real git repo
        git fetch --all
        git stash
    fi
    if [ -f "$updatefile" ]; then
        echo "Running $updatefile..."
        "$updatefile"
    fi
    res=$( git log HEAD..FETCH_HEAD --oneline )
    if [ -n "$res" ]; then
        echo -e
        echo "--- Updating $repo due to the following differences:"
        echo "$res"
        echo -e
        git pull --no-rebase -X theirs
    fi
elif [ -d "$parent/src/.hg" ]; then
    if [ -f "update" ]; then
        ./update
    fi
    if [ -f "external" ]; then
        rel=$( cat external )
        abs=$( _realpath.sh "$rel" )
        if [ -d "$abs" ]; then
            cd "$abs" || exit 1
        else
            echo "!!! $abs did not exist; ignoring ./external"
            cd "$parent/src" || exit 1
        fi
    else
        cd "$parent/src" || exit 1
    fi
    hg pull
    hg update
fi
# install
cd "$parent" || exit
if [ -f "$parent/.ignore_install" ]; then
    :
elif [ -f "$parent/.ignore_install_$(uname -s)" ]; then
    :
elif [ -f "$parent/install_$(uname -s)" ]; then
    ./"install_$(uname -s)"
elif [ -f "$parent/install" ]; then
    ./install
fi


# code ends here
