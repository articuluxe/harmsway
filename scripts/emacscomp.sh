#!/usr/bin/env sh
# -*- Mode: sh -*-
# emacscomp.sh --- byte-compile emacs lisp files
# Copyright (C) 2015  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, June 16, 2015
# Version: 1.0
# Modified Time-stamp: <2015-07-25 13:12:08 dharms>
# Keywords: emacs configuration

emacs=$EDITOR
dir=$(pwd)
user=$USER

if [ $# -gt 0 ] ; then
   dir="$1"
   shift
fi

cmd="(byte-recompile-directory \"$dir\" 0 t)"
echo "Compiling emacs files in directory $dir..."

$emacs --batch -u $user --eval "$cmd"

# emacscomp.sh ends here
