#!/bin/bash
# -*- Mode: sh -*-
# manage-repos.sh --- clone and update repos
# Copyright (C) 2017, 2019-2021  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 17, 2017
# Version: 1.0
# Modified Time-stamp: <2021-11-05 12:15:44 dharms>
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
    manage-repo.sh "$parent"
done

# code ends here
