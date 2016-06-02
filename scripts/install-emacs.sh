#!/usr/bin/env sh
# -*- Mode: sh -*-
# install-emacs.sh --- install emacs
# Copyright (C) 2015, 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Saturday, July 25, 2015
# Version: 1.0
# Modified Time-stamp: <2016-06-02 08:35:52 dan.harms>
# Modified by: Dan Harms
# Keywords: configuration

tar=${TAR:-`which tar`}
verbose=
emacs=$EDITOR
user=$USER
host=$(hostname -s)
int=emacs.tar
manifest=.bk_manifest
backup=emacs_bk.tar
orig_dir=${1:-~/src/harmsway}

# if [ $# -gt 0 ] ; then
#    verbose=v
#    shift
# fi

function backup_file
{
   if [ -f .emacs.d/$1 ] ; then
      echo Backing up $1
      $tar -rvf $backup .emacs.d/$1
   elif [ -d .emacs.d/$1 ] ; then
      echo Backing up directory $1
      $tar -rvf $backup .emacs.d/$1
   fi
}

if [ ! -d $orig_dir ]; then
   echo "$orig_dir does not exist; exiting..."
   exit 1
fi

echo "Tarring $orig_dir/.emacs.d into $int..."
cd $orig_dir

$tar c"$verbose"f $int --exclude=*.elc .emacs.d
if [ -d host/$host/.emacs.d ] ; then
   $tar u"$verbose"f $int --transform=s%host/$host%% host/$host/.emacs.d
fi
if [ -n $SITE -a -d site/$SITE/.emacs.d ] ; then
   $tar u"$verbose"f $int --transform=s%site/$SITE%% site/$SITE/.emacs.d
fi
mv -f $int ~

echo "Untarring $int into $HOME..."

cd ~

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

echo "...Done installing emacs.d"

# install-emacs.sh ends here
