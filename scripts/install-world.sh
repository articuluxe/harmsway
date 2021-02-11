#!/bin/bash
# -*- Mode: sh -*-
# install-world.sh --- install the world
# Copyright (C) 2016-2018, 2021  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, August 18, 2016
# Version: 1.0
# Modified Time-stamp: <2021-02-11 14:24:49 dharms>
# Modified by: Dan Harms
# Keywords: install

dir=${1:-~/src/harmsway}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1

tar-world.sh

if [ "$?" != 0 ]; then
    echo "!!! error executing tar-world.sh; aborting install"
    exit 1
fi

mv world.tar ~ && cd ~ && untar-world.sh world.tar

echo "install-world.sh done; press any key to continue..."
read -n1 -t5 key

# code ends here
