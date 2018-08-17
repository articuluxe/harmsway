#!/usr/bin/env sh
# -*- Mode: sh -*-
# unrootify.sh --- remove rootness from a file
# Copyright (C) 2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Friday, August 17, 2018
# Version: 1.0
# Modified Time-stamp: <2018-08-17 04:11:32 dan.harms>
# Modified by: Dan Harms
# Keywords: tools sh

person=$(id -nu)
group=$(id -ng)

if [ $# -lt 1 ] ; then
    echo "Need a file; exiting..."
    exit 1
fi

sudo chown "$person" "$1" && chgrp "$group" "$1"

# code ends here
