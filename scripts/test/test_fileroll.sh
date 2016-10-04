#!/usr/bin/env sh
# -*- Mode: sh -*-
# test_fileroll.sh --- test the file roll logic
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, October  4, 2016
# Version: 1.0
# Modified Time-stamp: <2016-10-04 16:12:04 dharms>
# Modified by: Dan Harms
# Keywords:

base=test
dir=testdir

sec=$(date '+%s')
digit=$(( sec % 5 ))

echo $digit > $dir/$base-stage

file-roller.sh $dir $base

# code ends here
