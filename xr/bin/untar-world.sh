#!/usr/bin/env sh
# -*- Mode: sh -*-
# untar-world.sh --- untar important files
# Copyright (C) 2015  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 18, 2015
# Version: 1.0
# Modified Time-stamp: <2015-08-05 10:08:32 dan.harms>
# Keywords: configuration

tar=tar
manifest=.bk_manifest
backup=emacs_bk.tar
input=

function backup_file
{
   if [ -f .emacs.d/$1 ] ; then
      echo Backing up $1
      $tar -rvf $backup .emacs.d/$1
   fi
}

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
tar --overwrite -xpf $input .emacs.d/$manifest
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
   tar czf .emacs.d.bk_$date.tgz --force-local .emacs.d
   # restore interesting files
   rm -rf .emacs.d
   mkdir .emacs.d
   if [ -r $backup ] ; then
      tar -xpf $backup
      rm -f $backup
   fi
fi

# remove and backup .ssh
if [ -d .ssh ] ; then
   $tar czf .ssh.bk_$date.tgz --force-local .ssh
   rm -rf .ssh
fi

echo About to unpack $input...
tar --overwrite -xpvf $input

# adjust .ssh
pushd ~/.ssh
ln -sf id_rsa_drh_npp id_rsa
ln -sf Dan.Harms.pub id_rsa.pub
chmod 600 ~/.ssh/id_rsa
popd

# remove intermediate directories, if empty
pushd ~
rmdir --ignore-fail-on-non-empty bash tcsh xr
# and byte-compile emacs
emacscomp.sh .emacs.d
popd

popd

# untar-world.sh ends here
