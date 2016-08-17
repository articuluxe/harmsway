#!/usr/bin/env sh
# -*- Mode: sh -*-
# _backup.sh --- backup talbot host
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, August  5, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-17 15:47:08 dan.harms>
# Modified by: Dan Harms
# Keywords: backup archive

tar=$TAR

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "!!! no tar available; quitting"
    exit 1
fi

. _archive_utils.sh

if [ $# -ne 2 ]; then
    exe=$(basename $0)
    echo "Usage: $exe <source> <dest>"
    exit 1
fi

if [ ! -r "$1" ]; then
    echo "!!! $1 is not readable; exiting..."
    exit 1
fi

# require absolute paths
is_absolute_path "$1"
if [ $? == 1 ]; then
    echo "!!! source directory $1 should be absolute; exiting..."
    exit 1
fi
is_absolute_path "$2"
if [ $? == 1 ]; then
    echo "!!! target directory $2 should be absolute; exiting..."
    exit 1
fi

stem=$(sanitize $1)
base=$2/$stem
mkdir -p $base
cd $base
shopt -s nullglob
files=( ${stem}_*.tar.gz )
max=${#files[@]}
if [[ $max > 999 ]]; then
    echo "!!! 1000 backups already exist; start over! Exiting..."
    exit 1
fi
digit=$(printf "%03d" $max)
file=${stem}_${digit}.tar.gz

parent=$(dirname $1)
child=$(basename $1)
cd $parent
tar -g "$base/$stem.snar" -czpf "$base/$file" "$child"
echo -e
echo "Created a level $max backup of $1 in $base"
echo -e
tar -G -tvvpf $base/$file

# code ends here
