#!/bin/sh
# -*- Mode: sh -*-
# repo-init.sh --- select a repo to initialize
# Copyright (C) 2016, 2018, 2021  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, September 15, 2016
# Version: 1.0
# Modified Time-stamp: <2021-03-25 17:10:42 dharms>
# Modified by: Dan Harms
# Keywords: repo src

echo "Install which project?"
proj=$(_dirselect.sh ~/config/repos "Select a project: ")
if [ -z "$proj" ]; then
    echo "!!! No project selected; exiting..."
    exit 1
fi

echo "Which version of $proj should be installed?"
ver=$(_dirselect.sh "$HOME/config/repos/$proj" "Select a version: ")
if [ -z "$ver" ]; then
    echo "!!! No version of $proj selected; exiting..."
    exit 1
fi

target=~/config/repos/"$proj"/"$ver"/"$proj"-init.sh
if [ ! -f "$target" ]; then
    echo "!!! $target not present; exiting..."
    exit 1
fi

"$target" "$proj" "$ver" "$1"

# code ends here
