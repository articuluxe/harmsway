#!/usr/bin/env sh
# -*- Mode: sh -*-
# mock-install.sh --- install mock script run
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, March  1, 2016
# Version: 1.0
# Modified Time-stamp: <2016-03-01 16:24:57 dan.harms>
# Modified by: Dan Harms
# Keywords: mock scripting

subdir=$2

mkdir -p $subdir
tar -C $subdir -xzf $1

# code ends here
