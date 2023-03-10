#!/bin/bash
# -*- Mode: sh -*-
# tar-deployment.sh --- tar a repo for deployment
# Copyright (C) 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 10, 2023
# Version: 1.0
# Modified Time-stamp: <2023-03-10 15:01:17 dharms>
# Modified by: Dan Harms
# Keywords:

# All paths here should be absolute
dir="$1"
archive="$2"

if [ ! -d "$dir" ]; then
    echo "!!! $dir is not a valid directory; exiting..."
    exit 1
fi

tar=$TAR
if [ -z "$tar" ]; then
    tar=$(command -v tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "!!! no tar available; exiting..."
    exit 1
fi

repo=$(basename "$dir")
parent=$(dirname "$dir")

cd "$parent" || exit 1

echo -e
echo "-*- $repo -*-"
echo "Deploying"

"$tar" uf "$archive" --exclude=local --exclude=bld\* --exclude=.git --transform=s%"$repo"%deploy/"$repo"% "$repo"
echo -e

# code ends here
