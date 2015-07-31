#!/usr/bin/env sh
# -*- Mode: sh -*-
# install-emacs.sh --- install emacs
# Copyright (C) 2015  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Saturday, July 25, 2015
# Version: 1.0
# Modified Time-stamp: <2015-07-31 11:56:08 dan.harms>
# Modified by: Dan Harms
# Keywords: configuration

tar=$TAR
verbose=
emacs=$EDITOR
user=$USER
int=emacs.tar
manifest=.bk_manifest
backup=emacs_bk.tar

if [ $# -gt 0 ] ; then
   verbose=v
   shift
fi

echo "Tarring .emacs.d into $int..."
$tar c"$verbose"f $int --exclude=*.elc .emacs.d
mv -f $int ~

echo "Untarring $int into $HOME..."

pushd ~

if [ -d .emacs.d ] ; then
   rm -f $backup
   # todo
   rm -rf .emacs.d
   mkdir .emacs.d
   if [ -r $backup ] ; then
      $tar -xvpf $backup
      rm -f $backup
   fi
fi

tar --overwrite -x"$verbose"pf $int
rm -f $int

emacscomp.sh .emacs.d
popd

echo "...Done installing emacs.d"

# install-emacs.sh ends here
