#!/usr/bin/env sh
# -*- Mode: sh -*-
# tar-world.sh --- tar up important configuration files
# Copyright (C) 2015, 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, May 29, 2015
# Version: 1.0
# Modified Time-stamp: <2016-01-16 15:22:58 dharms>
# Modified by: Dan Harms
# Keywords: configuration

tar=$TAR
os=$(uname)
host=$(hostname -s)
site=talbot
dest=world.tar
verbose=

if [ -z "$tar" ]; then
   tar=$(which tar)
   echo "Using $tar"
fi
if [ -z "$tar" ]; then
   echo "! no tar available; quitting"
   exit 1
fi
if [ $# -gt 0 ] ; then
   dest=$1
   shift
fi

echo Generating $dest...

if [ -f "$dest" ] ; then
   rm -f $dest
fi

$tar c"$verbose"f $dest config doc src .gdbinit
$tar u"$verbose"f $dest --exclude=*.elc .emacs.d
$tar u"$verbose"f $dest --transform=s/scripts/bin/ scripts
$tar u"$verbose"f $dest --transform=s/bash\\/// bash
$tar u"$verbose"f $dest --transform=s/tcsh\\/// tcsh
$tar u"$verbose"f $dest --transform=s%os/$os\\/%% os/$os
$tar u"$verbose"f $dest --transform=s%host/$host\\/%% host/$host
$tar u"$verbose"f $dest --transform=s%site/$site\\/%% site/$site

echo ...done generating $dest

# tar-world.sh ends here
