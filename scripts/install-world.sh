#!/bin/bash
# -*- Mode: sh -*-
# install-world.sh --- install the world
# Copyright (C) 2016-2018, 2021-2022, 2026  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, August 18, 2016
# Version: 1.0
# Modified Time-stamp: <2026-01-05 11:41:30 dharms>
# Modified by: Dan Harms
# Keywords: install

dir=${1:-~/src/harmsway}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1
rev=$( git rev-parse --short HEAD )

echo "$(date) - $rev" >> .harmsway.last-tarred

tar-world.sh

if [ "$?" != 0 ]; then
    echo "!!! error executing tar-world.sh; aborting install"
    exit 1
fi

mv world.tar ~ && cd ~ && untar-world.sh world.tar

if command -v manage-permissions.sh > /dev/null ; then
    manage-permissions.sh
else
    echo "manage-permissions.sh not found; skipping."
fi

echo "$(date) - $rev" >> .harmsway.last-installed

echo "install-world.sh done; press any key to continue..."
read -n1 -t5 key

# code ends here
