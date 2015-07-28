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
   tar czf .emacs.d.bk_$date.tgz --force-local .emacs.d
   # restore interesting files
   rm -rf .emacs.d
   mkdir .emacs.d
   tar -C .emacs.d -xpf $int
   rm -f $int
fi
# remove and backup .ssh
if [ -d .ssh ] ; then
   $tar czf .ssh.bk_$date.tgz --force-local .ssh
   rm -rf .ssh
fi

echo About to unpack $input...
tar -C ~ --overwrite -xpvf $input

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
