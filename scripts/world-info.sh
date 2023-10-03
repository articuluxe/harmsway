#!/bin/sh
# -*- Mode: sh -*-
# world-info.sh --- get info about world
# Copyright (C) 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, October  3, 2023
# Version: 1.0
# Modified Time-stamp: <2023-10-03 11:33:25 dharms>
# Modified by: Dan Harms
# Keywords:

test -e

registry=~/.harmsway.last-installed
if [ ! -f "$registry"  ]; then
    echo "Install registry not found, exiting..."
    exit 1
fi

if [ ! -d ~/src/harmsway ]; then
    echo "Harmsway not installed, exiting..."
    exit 1
fi
cd ~/src/harmsway || exit 1

last=$( tail -1 $registry | cut -d'-' -f2 )
curr=$( git rev-parse --short HEAD )

git log $last..$curr --oneline

# code ends here
