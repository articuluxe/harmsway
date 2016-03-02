#!/usr/bin/env sh
# -*- Mode: sh -*-
# mock-install.sh --- install mock script run
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, March  1, 2016
# Version: 1.0
# Modified Time-stamp: <2016-03-02 16:33:06 dan.harms>
# Modified by: Dan Harms
# Keywords: mock scripting

archive=mock.tar.gz
subdir=${PWD##*/}

cd ..
mkdir -p $subdir
tar --overwrite -C $subdir -xzf $archive

cd $subdir
mtime=$(stat -c %y xr-snap)
echo "Unzipped $archive; snap dated $mtime"

# code ends here
