#!/usr/bin/env bash
# -*- Mode: sh -*-
# emacscomp.sh --- byte-compile emacs lisp files
# Copyright (C) 2015-2017  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, June 16, 2015
# Version: 1.0
# Modified Time-stamp: <2017-04-14 07:14:17 dharms>
# Keywords: emacs configuration

emacs=$EDITOR
dir=$(pwd)
user=$USER
date=$(date '+%F_%T' | tr ':' '-')
logname=.emacs-install-log-$date.log

if [ $# -gt 0 ] ; then
   dir="$1"
   shift
fi

cmd="(byte-recompile-directory \"$dir\" 0 t)"
echo "Compiling emacs files in directory $dir..."
echo -e

$emacs --batch -u "$user" --eval "$cmd" &> "$logname"
#Done (Total of 292 files compiled, 2 failed in 33 directories)
grep -i 'error' "$logname"
grep -e '^Done' "$logname"
echo -e

gzip "$logname"

# emacscomp.sh ends here
