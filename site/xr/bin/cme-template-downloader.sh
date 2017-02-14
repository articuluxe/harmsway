#!/usr/bin/env sh
# -*- Mode: sh -*-
# cme-template-downloader.sh --- download and check a file
# Copyright (C) 2016-2017  Dan Harms (dharms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, October  3, 2016
# Version: 1.0
# Modified Time-stamp: <2017-02-14 14:17:03 dan.harms>
# Modified by: Dan Harms
# Keywords:

name=templates_FixBinary-prod.xml
loc=ftp://ftp.cmegroup.com/SBEFix/Production/Templates/templates_FixBinary.xml
dir=~/cme-templates

mkdir -p $dir
cd $dir
wget -O $name-stage $loc > /tmp/cme-template.downloader.log 2>&1
file-roller.sh $dir $name -stage -b\ -I\ '<configuration.*">'

# code ends here
