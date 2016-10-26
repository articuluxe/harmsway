#!/usr/bin/env sh
# -*- Mode: sh -*-
# cme-config-downloader.sh --- download and check a file
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, October  3, 2016
# Version: 1.0
# Modified Time-stamp: <2016-10-26 11:10:05 dan.harms>
# Modified by: Dan Harms
# Keywords:

name=config-prod.xml
loc=ftp://ftp.cmegroup.com/SBEFix/Production/Configuration/config.xml
dir=~/cme-configs

mkdir -p $dir
cd $dir
wget -O $name-stage $loc > /tmp/cme-config-downloader.log 2>&1
file-roller.sh $dir $name

# code ends here
