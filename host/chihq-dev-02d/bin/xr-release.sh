#!/usr/bin/env sh
# -*- Mode: sh -*-
# xr-release.sh --- distribute xr release to hosts
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 30, 2016
# Version: 1.0
# Modified Time-stamp: <2016-06-10 13:43:10 dan.harms>
# Modified by: Dan Harms
# Keywords: xr config

host=$(hostname -s)
hosts="
$VM
hq-snapdev-77v
chihq-xmock-01h
chihq-xmock-02c
carvzn-snap-06h
chihq-benchmark-32e
"

cd ~/src/harmsway/
host/$host/bin/xr-tar-world.sh

for host in $hosts; do
   echo "Copying xr-world.tar to $host"
   scp xr-world.tar $host:~
done

# code ends here
