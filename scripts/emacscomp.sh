#!/usr/bin/env sh
# -*- Mode: sh -*-
# emacscomp.sh --- byte-compile emacs lisp files
# Copyright (C) 2015, 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, June 16, 2015
# Version: 1.0
# Modified Time-stamp: <2016-02-04 17:07:56 dan.harms>
# Keywords: emacs configuration

emacs=$EDITOR
dir=$(pwd)
user=$USER
date=$(date '+%F_%T' | tr ':' '-')

if [ $# -gt 0 ] ; then
   dir="$1"
   shift
fi

cmd="(byte-recompile-directory \"$dir\" 0 t)"
echo "Compiling emacs files in directory $dir..."
echo -e '\n'

$emacs --batch -u $user --eval "$cmd" &> emacs-install-log-$date.log
#Done (Total of 292 files compiled, 2 failed in 33 directories)
grep -i 'error' emacs-install-log-$date.log
grep -e '^Done' emacs-install-log-$date.log
echo -e '\n'

gzip emacs-install-log-$date.log

# emacscomp.sh ends here
