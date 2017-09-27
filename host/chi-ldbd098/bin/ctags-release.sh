#!/usr/bin/env sh
# -*- Mode: sh -*-
# ctags-release.sh --- distribute ctags release to hosts
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 30, 2016
# Version: 1.0
# Modified Time-stamp: <2016-12-14 14:49:01 dan.harms>
# Modified by: Dan Harms
# Keywords: xr config

bundle=ctags.bundle
hosts="
$VM
carvzn-snap-06h
"

cd ~/src/ctags/
rm -f $bundle
git bundle create $bundle master

for host in $hosts; do
   scp $bundle $host:~/src/$bundle
done

# code ends here
