#!/usr/bin/env sh
# -*- Mode: sh -*-
# kitty-gen.sh --- generate a kitty config file
# Copyright (C) 2018  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, November  6, 2018
# Version: 1.0
# Modified Time-stamp: <2018-11-06 12:49:04 dan.harms>
# Modified by: Dan Harms
# Keywords: tools

src=$1
sess=$2
base=$(echo $(basename "$src") | sed -e 's/.\{1,2\}. \(.*\).reg/\1/')
dst="$sess"-$(echo "$base" | sed -e 's/ //g')

[ -f "$sess".template ] || ( echo "$sess.template missing!" && exit 1 )

cp "$sess".template "$dst"
sed -n 's/"Colour\(.*\)"="\(.*\)"/Colour\1\\\2\\/ p' < "$src" >> "$dst"

echo "Created $dst"

# sed -nf reg-to-kitty.sed

# code ends here
