#!/usr/bin/env sh
# -*- Mode: sh -*-
# xr-install-world.sh --- install the world, xr style
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, August 18, 2016
# Version: 1.0
# Modified Time-stamp: <2016-09-09 13:44:04 dan.harms>
# Modified by: Dan Harms
# Keywords: xr install

file=xr-world.tar
dir=${1:-$HOME}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi
cd $dir
if [ ! -f "$file" ]; then
    echo "!!! $file is not present; exiting..."
    exit 1
fi

xr-untar-world.sh $file

# code ends here
