#!/usr/bin/env sh

# -*- Mode: sh -*-
# pipe-to-emacs.sh --- pipe from stdin to emacs
# Copyright (C) 2015  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Thursday, September 24, 2015
# Version: 1.0
# Modified Time-stamp: <2015-02-26 13:20:30 dharms>
# Modified by: Dan Harms
# Keywords: emacs

function _emacsfun {
   emacs $@
   # emacsclient -c -n $@
}

# example: echo "hello world" | emacspipe -
function emacspipe {
   if [[ $1 == - ]]; then
      tempfile=$(mktemp emacs-stdin-$USER.XXXXXXXX --tmpdir)
      cat - > $tempfile
      _emacsfun -e "(progn (find-file \"$tempfile\")  \
                           (set-visited-file-name nil) \
                           (rename-buffer \"*stdin*\" t))\
                   " 2>&1 > /dev/null
   else
      _emacsfun "$@"
   fi

}

# pipe-to-emacs.sh ends here
