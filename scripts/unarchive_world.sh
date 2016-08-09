#!/usr/bin/env sh
# -*- Mode: sh -*-
# unarchive_world.sh --- unarchive a world archive
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-09 15:24:50 dan.harms>
# Modified by: Dan Harms
# Keywords: archive backup

shopt -s nullglob

stem=unarchive_world.sh
stemlen=#stem
echo stemlen $stemlen $stem
files=(stem_*.txt)

max=#files[@]
echo max is $max

if ((#files[@] == 0)); then
    echo files is empty
else
    echo files is files[@]
fi

for i in files[@]; do
    num=i:$stemlen+1:3
    echo $i: $num
done

# eval lastidx=$$#
# tmp=lastidx#*[!0-9][0-9]

# echo $lastidx
# echo $tmp

# code ends here
