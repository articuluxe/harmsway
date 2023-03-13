#!/bin/bash
# -*- Mode: sh -*-
# untar-deployment.sh --- untar a deployed repo
# Copyright (C) 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, March 13, 2023
# Version: 1.0
# Modified Time-stamp: <2023-03-13 13:42:42 dharms>
# Modified by: Dan Harms
# Keywords:

# path should be absolute
dir=${1:-$(pwd)}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1

repo=$( basename "$dir" )
base="$dir/remote"

echo -e
echo "-*- $repo -*-"

# build
if [ -f "$base/.ignore_build" ]; then
    :
elif [ -f "$base/.ignore_build_$(uname -s)" ]; then
    :
elif [ -f "$base/build_$(uname -s)" ]; then
    "$base/build_$(uname -s)"
elif [ -f "$base/build" ]; then
    "$base/build"
fi
# install
if [ -f "$base/.ignore_install" ]; then
    :
elif [ -f "$base/.ignore_install_$(uname -s)" ]; then
    :
elif [ -f "$base/install_$(uname -s)" ]; then
    "$base/install_$(uname -s)"
elif [ -f "$base/install" ]; then
    "$base/install"
fi

# code ends here
