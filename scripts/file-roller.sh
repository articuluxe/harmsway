#!/bin/bash
# -*- Mode: sh -*-
# file-roller.sh --- roll a file if it changes over time
# Copyright (C) 2016-2017  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, October  3, 2016
# Version: 1.0
# Modified Time-stamp: <2017-08-24 17:29:39 dharms>
# Modified by: Dan Harms
# Keywords: file roll diff

if [ $# -lt 2 ]; then
    echo "Usage: $0 <dir> <file> [stage-extension] [diff-options]"
    exit 1
fi

dir=$1
base=$2
ext=${3:-"-stage"}
diffopts=${4:-"-b"}
maxoldfiles=${5:-25}
stage=$base$ext
date=$( date '+%Y%m%d-%H%M%S' )
fullname=$base-$date

if [ ! -d "$dir" ]; then
    echo "!!! $dir does not exist; exiting..."
    exit 1
fi

cd "$dir"
# dir needs to exist
# stage file needs to already have been copied to dir

if [ ! -f "$base" ]; then
    # first run; save the initial revision
    mv "$stage" "$fullname"
    ln -sf "$fullname" "$base"
    exit 0
fi

output=$( diff $diffopts "$base" "$stage" )
if [ $? == 1 ]; then
    # there was an update
    orig=$(readlink "$base")
    gzip "$orig"
    mv "$stage" "$fullname"
    ln -sf "$fullname" "$base"
    echo "<<<<< $base has been updated as of $date >>>>>"
    # remove old files
    oldfiles=( "$base"-*.gz )
    shopt -u nullglob
    num=${#oldfiles[@]}
    if [[ $num -gt $maxoldfiles ]]; then
        mv "${oldfiles[0]}" /tmp/"${oldfiles[0]}"
        echo -e
        echo "Eradicated oldest archived file to /tmp/${oldfiles[0]}"
    fi
    # echo difference
    echo -e
    echo "Diff:"
    echo -e
    echo "$output"
fi

rm -f "$stage"

# code ends here
