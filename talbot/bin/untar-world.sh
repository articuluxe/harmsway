#!/usr/bin/env sh
# -*- Mode: sh -*-
# untar-world.sh --- untar important files
# Copyright (C) 2015  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 18, 2015
# Version: 1.0
# Modified Time-stamp: <2015-07-28 06:03:06 dharms>
# Keywords: configuration

tar=$TAR
int=emacs_int.tar
input=

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
if [ -d .emacs.d ] ; then
   # copy interesting files
   tar cf $int .emacs.d/ac-comphist.dat
   tar uf $int .emacs.d/recentf
   tar uf $int .emacs.d/smex-items
   tar uf $int .emacs.d/history
   tar uf $int .emacs.d/autosaves
   tar uf $int .emacs.d/backups
   # backup for posterity
   $tar czf .emacs.d.bk_$date.tgz --force-local .emacs.d
   # restore interesting files
   rm -rf .emacs.d
   mkdir .emacs.d
   tar -C .emacs.d -xpf $int
   rm -f $int
fi

echo About to unpack $input...
$tar -C ~ --overwrite -xpvf $input

# remove intermediate directories, if empty
rmdir -p bash tcsh talbot
# and byte-compile emacs
emacscomp.sh .emacs.d
popd

# untar-world.sh ends here
