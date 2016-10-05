#!/usr/bin/env sh
# -*- Mode: sh -*-
# cme-config-downloader.sh --- download and check a file
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, October  3, 2016
# Version: 1.0
# Modified Time-stamp: <2016-10-05 06:38:19 dharms>
# Modified by: Dan Harms
# Keywords:

name=config-prod.xml
loc=ftp://ftp.cmegroup.com/SBEFix/Production/Configuration/config.xml
dir=?

mkdir -p $dir
cd $dir
wget -O $name $loc
file-roller.sh $dir $name

# code ends here
