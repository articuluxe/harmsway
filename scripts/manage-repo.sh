#!/bin/bash
# -*- Mode: sh -*-
# manage-repo.sh --- clone and update a repo
# Copyright (C) 2021, 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, November  5, 2021
# Version: 1.0
# Modified Time-stamp: <2023-03-10 12:01:17 dharms>
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
base="$parent/local"

# clone
if [ ! -d "$parent/src" ] && [ -f "$base/clone" ]; then
    echo "Cloning $repo"
    local/clone
fi

# update
if [ -d "$parent/src/.git" ] \
       || [ -d "$parent/src/.git-ignore" ] \
       || [ -d "$parent/src/.git-ignore_$(uname -s)" ]; then
    if [ -f "$base/update" ]; then
        updatefile="$base/update"
    fi
    if [ -f "$base/external" ]; then
        rel=$( cat local/external )
        abs=$( _realpath.sh "$rel" )
        if [ -d "$abs" ]; then
            echo "Changing path based on local/external to $abs"
            cd "$abs" || exit 1
        else
            echo "!!! $abs did not exist; ignoring local/external"
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
    if [ -d "$parent/src/.git" ]; then
        # only run these commands for a real git repo
        res=$( git log HEAD..FETCH_HEAD --oneline )
        if [ -n "$res" ]; then
            echo -e
            echo "--- Updating $repo due to the following differences:"
            echo "$res"
            echo -e
            git rebase -X theirs
        fi
    fi
elif [ -d "$parent/src/.hg" ]; then
    if [ -f "$base/update" ]; then
        local/update
    fi
    if [ -f "$base/external" ]; then
        rel=$( cat local/external )
        abs=$( _realpath.sh "$rel" )
        if [ -d "$abs" ]; then
            cd "$abs" || exit 1
        else
            echo "!!! $abs did not exist; ignoring local/external"
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
if [ -f "$base/.ignore_install" ]; then
    :
elif [ -f "$base/.ignore_install_$(uname -s)" ]; then
    :
elif [ -f "$base/install_$(uname -s)" ]; then
    local/"install_$(uname -s)"
elif [ -f "$base/install" ]; then
    local/install
fi

# code ends here
