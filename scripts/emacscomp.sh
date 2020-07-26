#!/bin/sh
# -*- Mode: sh -*-
# emacscomp.sh --- byte-compile emacs lisp files
# Copyright (C) 2015-2017, 2020  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, June 16, 2015
# Version: 1.0
# Modified Time-stamp: <2020-07-23 11:54:31 dharms>
# Keywords: emacs configuration

emacs=$EDITOR
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

mkdir -p "$logdir"

cmd="(byte-recompile-directory \"$dir\" 0 t)"
echo "Compiling emacs files in directory $dir..."
echo -e

$emacs --batch -u "$user" --eval "$cmd" &> "$log"
#Done (Total of 292 files compiled, 2 failed in 33 directories)
grep -i 'error' "$log"
grep -e '^Done' "$log"
echo -e

gzip "$log"

# emacscomp.sh ends here
