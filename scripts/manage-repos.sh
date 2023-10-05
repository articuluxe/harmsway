#!/bin/bash
# -*- Mode: sh -*-
# manage-repos.sh --- clone and update repos
# Copyright (C) 2017, 2019-2021, 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 17, 2017
# Version: 1.0
# Modified Time-stamp: <2023-10-05 10:51:54 dharms>
# Modified by: Dan Harms
# Keywords: git repo

dir=${1:-~/src/harmsway/repos}

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

cd "$dir" || exit 1

# ensure _ sorts first
find "$dir" -type f -ipath "*local/clone" | LC_COLLATE=C sort | while read -r fname; do
    subparent=$( dirname "$fname" ) # local
    parent=$( dirname "$subparent" )
    manage-repo.sh "$parent"
done

# code ends here
