#!/usr/bin/env sh
# -*- Mode: sh -*-
# talbot-tar-world.sh --- tar up important configuration files
# Copyright (C) 2015  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, May 29, 2015
# Version: 1.0
# Modified Time-stamp: <2015-07-25 16:20:12 dharms>
# Keywords: configuration

tar=$TAR
dest=world.tar
verbose=

if [ $# -gt 0 ] ; then
   dest=$1
   shift
fi

echo Generating $dest...

if [ -f "$dest" ] ; then
   rm -f $dest
fi

$tar c"$verbose"f $dest config doc src .gitignore .gdbinit
$tar u"$verbose"f $dest --transform=s/scripts/bin/ scripts
$tar u"$verbose"f $dest --transform=s/bash\\/// bash
$tar u"$verbose"f $dest --transform=s/tcsh\\/// tcsh
$tar u"$verbose"f $dest --transform=s/talbot\\/// talbot
$tar u"$verbose"f $dest --exclude=*.elc .emacs.d

echo ...done generating $dest

# talbot-tar-world.sh ends here
