#!/usr/bin/env sh
# -*- Mode: sh -*-
# file_counter.sh --- count files in a dir
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, October 28, 2016
# Version: 1.0
# Modified Time-stamp: <2016-11-08 07:03:32 dharms>
# Modified by: Dan Harms
# Keywords: mac photos

for i in {1..8393}; do
    digit=$( printf "%04d" $i )
    name="IMG_$digit"
    result=$( ls $name.* &> /dev/null )
    [ $? == 1 ] && echo "$name is missing"
done


# code ends here
