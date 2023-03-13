#!/bin/sh
# -*- Mode: sh -*-
# untar-deployments.sh --- install deployed repos
# Copyright (C) 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, March 13, 2023
# Version: 1.0
# Modified Time-stamp: <2023-03-13 14:28:58 dharms>
# Modified by: Dan Harms
# Keywords:

dir=${1:-~/deploy}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1

find "$dir" -type d -path "*remote" | sort | while read -r dir; do
    parent=$( dirname "$dir" )
    untar-deployment.sh "$parent"
done

# code ends here
