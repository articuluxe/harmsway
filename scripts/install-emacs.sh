#!/usr/bin/env sh
# -*- Mode: sh -*-
# install-emacs.sh --- install emacs
# Copyright (C) 2015  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Saturday, July 25, 2015
# Version: 1.0
# Modified Time-stamp: <2015-08-03 05:44:28 dharms>
# Modified by: Dan Harms
# Keywords: configuration

tar=$TAR
verbose=
emacs=$EDITOR
user=$USER
int=emacs.tar
manifest=.bk_manifest
backup=emacs_bk.tar
orig_dir=`pwd`

if [ $# -gt 0 ] ; then
   verbose=v
   shift
fi

function backup_file
{
   if [ -f .emacs.d/$1 ] ; then
      echo Backing up $1
      $tar -rvf $backup .emacs.d/$1
   fi
}

echo "Tarring .emacs.d into $int..."
$tar c"$verbose"f $int --exclude=*.elc .emacs.d
mv -f $int ~

echo "Untarring $int into $HOME..."

pushd ~

if [ -d .emacs.d ] ; then
   rm -f $backup
   files=(`cat $orig_dir/.emacs.d/$manifest`)
   numfiles=${#files[*]}
   i=0
   while [ $i -lt $numfiles ]
   do
      backup_file ${files[$i]}
      i=$(( $i+1 ))
   done
   rm -rf .emacs.d
   mkdir .emacs.d
   if [ -r $backup ] ; then
      $tar -xvpf $backup
      rm -f $backup
   fi
fi

tar -x"$verbose"pf $int
rm -f $int

emacscomp.sh .emacs.d
popd

echo "...Done installing emacs.d"

# install-emacs.sh ends here
