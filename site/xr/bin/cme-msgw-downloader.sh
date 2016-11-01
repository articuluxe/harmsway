#!/usr/bin/env sh
# -*- Mode: sh -*-
# cme-msgw-downloader.sh --- download and check file
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, November  1, 2016
# Version: 1.0
# Modified Time-stamp: <2016-11-01 15:03:45 dan.harms>
# Modified by: Dan Harms
# Keywords: msgw

dir=~/cme-msgw
name=MSGW_Config.xml
user=cmeconfig
pwd="G3t(0nnect3d"
site=sftpng.cmegroup.com
env=Production
loc=/MSGW/$env/Configuration/$name

mkdir -p $dir
cd $dir
mkdir -p stage
cd stage
sshpass -p $pwd sftp $user@$site:$loc > /tmp/cme-msgw-downloader.log 2>&1
mv $name ../$name-stage
cd ..
file-roller.sh $dir $name

# code ends here
