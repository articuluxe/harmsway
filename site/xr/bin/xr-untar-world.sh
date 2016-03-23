#!/usr/bin/env sh
# -*- Mode: sh -*-
# xr-untar-world.sh --- untar important files
# Copyright (C) 2015, 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 18, 2015
# Version: 1.0
# Modified Time-stamp: <2016-03-23 16:58:19 dan.harms>
# Modified by: Dan Harms
# Keywords: configuration

tar=tar
manifest=.bk_manifest
backup=emacs_bk.tar
os=$(uname)
host=$(hostname -s)
site=xr
input=

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

if [ -z "$tar" ]; then
   tar=$(which tar)
   echo "Using $tar"
fi
if [ -z "$tar" ]; then
   echo "! no tar available; quitting"
   exit 1
fi
if [ $# -gt 0 ] ; then
   input=$1
   shift
fi

if [ "x$input" = "x" ] ; then
   echo "Error: need an input file."
   exit
fi

date=$(date '+%F_%T' | tr ':' '-')

pushd ~

# there's an existing .emacs.d
$tar --overwrite -xpf $input .emacs.d/$manifest
if [ -d .emacs.d ] && [ -f .emacs.d/$manifest ] ; then
   rm -f $backup
   files=(`cat .emacs.d/$manifest`)
   numfiles=${#files[*]}
   i=0
   while [ $i -lt $numfiles ]
   do
      backup_file ${files[$i]}
      i=$(( $i+1 ))
   done
   # backup for posterity
   $tar czf .emacs.d.bk_$date.tgz --force-local .emacs.d
   # restore interesting files
   rm -rf .emacs.d
   mkdir .emacs.d
   if [ -r $backup ] ; then
      $tar -xpf $backup
      rm -f $backup
   fi
fi

# remove and backup .ssh
if [ -d .ssh ] ; then
   $tar czf .ssh.bk_$date.tgz --force-local .ssh
   rm -rf .ssh
fi

echo About to unpack $input...
$tar --overwrite -xpvf $input

# adjust .ssh
pushd ~/.ssh
ln -sf id_rsa_drh_npp id_rsa
ln -sf Dan.Harms.pub id_rsa.pub
chmod 600 ~/.ssh/id_rsa
popd

# remove intermediate directories, if empty
rmdir -p --ignore-fail-on-non-empty bash tcsh os/$os site/$site
# protect .netrc
if [ -f .netrc ] ; then
   chmod 600 .netrc
fi
# rename host file as part of distribution
if [ -f .bash_host ]; then
   mv .bash_host .bash_$host
fi
# and byte-compile emacs
emacscomp.sh .emacs.d

popd

# untar-world.sh ends here
