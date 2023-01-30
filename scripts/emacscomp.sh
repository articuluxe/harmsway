#!/bin/bash
# -*- Mode: sh -*-
# emacscomp.sh --- byte-compile emacs lisp files
# Copyright (C) 2015-2017, 2020-2021, 2023  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, June 16, 2015
# Version: 1.0
# Modified Time-stamp: <2023-01-30 16:35:02 dharms>
# Keywords: emacs configuration

exe=$EMACS_BIN
dir=$(pwd)
user=$USER
date=$(date '+%F_%T' | tr ':' '-')
logdir=.install-logs
logname=.emacs-install-log-$date.log
log="$logdir/$logname"

if [ $# -gt 0 ] ; then
    dir="$1"
    shift
fi

if [ -z "$exe" ]; then
    echo "!!! Missing emacs executable; skipping compile..."
    return 1
fi

mkdir -p "$logdir"

cmd="(byte-recompile-directory \"$dir\" 0 t)"
echo "Using $exe to compile files in directory $dir..."
echo -e

$exe --batch -u "$user" --eval "$cmd" > "$log" 2>&1
#Done (Total of 292 files compiled, 2 failed in 33 directories)
grep -i 'error' "$log"
grep -e '^Done' "$log"
echo -e

gzip "$log"

# emacscomp.sh ends here
