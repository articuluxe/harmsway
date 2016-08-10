#!/usr/bin/env sh
# -*- Mode: sh -*-
# _backup.sh --- backup talbot host
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, August  5, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-09 16:32:55 dharms>
# Modified by: Dan Harms
# Keywords: backup archive

tar=$TAR

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "! no tar available; quitting"
    exit 1
fi

date=$(date '+%Y%m%d')
time=$(date '+%H%M%S')
when=$date-$time

if [ $# -ne 2 ]; then
    echo "Usage: $0 <source> <dest>"
    exit 1
fi

if [ ! -r "$1" ]; then
    echo "$1 is not readable; exiting..."
    exit 1
fi

echo "Backing up $1 to $2"
stem=$(sanitize $1)
base=$2/$stem
mkdir -p $base
digit=0
tar -g $base/$stem.snar -czpf $base/$stem_${digit}.tar.gz $1

function sanitize {
    clean=$1
    # remove any leading and trailing slashes
    clean=$(echo $clean | sed -e 's$^/$$')
    clean=$(echo $clean | sed -e 's%/$%%')
    # replace slashes with dots
    clean=${clean////.}
    # replace underscores with dots
    clean=${clean//_/.}
    # allow only alphanumeric + .
    clean=${clean//[^a-zA-Z0-9]/.}
    # collapse consecutive dots
    clean=$(echo $clean | tr -s '\0-\255')
    echo "$clean"
}

# code ends here
