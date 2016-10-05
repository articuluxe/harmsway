#!/usr/bin/env sh
# -*- Mode: sh -*-
# cme-template-downloader.sh --- download and check a file
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, October  3, 2016
# Version: 1.0
# Modified Time-stamp: <2016-10-05 06:37:35 dharms>
# Modified by: Dan Harms
# Keywords:

name=templates_FixBinary-prod.xml
loc=ftp://ftp.cmegroup.com/SBEFix/Production/Templates/templates_FixBinary.xml
dir=?

mkdir -p $dir
cd $dir
wget -O $name $loc
file-roller.sh $dir $name

# code ends here
