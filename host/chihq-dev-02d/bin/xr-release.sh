#!/usr/bin/env bash
# -*- Mode: sh -*-
# xr-release.sh --- distribute xr release to hosts
# Copyright (C) 2016-2017  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 30, 2016
# Version: 1.0
# Modified Time-stamp: <2017-06-08 11:35:59 dan.harms>
# Modified by: Dan Harms
# Keywords: xr config

hostfile=~/src/harmsway/site/xr/.emacs.d/settings/host/hosts/xr

if [ -f "$hostfile" ]; then
    while read -r host description
    do
        if [ "${host:0:1}" != "#" ]; then
            hosts+=($host)
        fi
    done <"$hostfile"
else
   echo "No host file $hostfile; output will not be distributed"
fi

cd ~/src/harmsway/
host/chihq-dev-02d/bin/xr-tar-world.sh

numhosts=${#hosts[*]}
i=0
while [ $i -lt $numhosts ]
do
   echo "Copying xr-world.tar to ${hosts[$i]}"
   scp xr-world.tar ${hosts[$i]}:~
   i=$(( $i+1 ))
done

# code ends here
