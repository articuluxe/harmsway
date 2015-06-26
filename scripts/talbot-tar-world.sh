#!/usr/bin/env sh
# -*- Mode: sh -*-
# talbot-tar-world.sh --- tar up important configuration files
# Copyright (C) 2015  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, May 29, 2015
# Version: 1.0
# Modified Time-stamp: <2015-06-26 05:35:22 dharms>
# Keywords: configuration

tar=/usr/bin/gnutar
dest=world.tgz
verbose=

if [ $# -gt 0 ] ; then
   dest=$1
   shift
fi

echo Generating $dest...

if [ -f "$dest" ] ; then
   rm -f $dest
fi

$tar c"$verbose"zf $dest config doc .gitignore
$tar u"$verbose"zf $dest --transform=s/scripts/bin/ scripts
$tar u"$verbose"zf $dest --transform=s/bash\\/// bash
$tar u"$verbose"zf $dest --transform=s/tcsh\\/// tcsh
$tar u"$verbose"zf $dest --transform=s/talbot\\/// talbot
$tar u"$verbose"zf $dest --exclude=*.elc .emacs.d

echo ...done generating $dest

# talbot-tar-world.sh ends here
