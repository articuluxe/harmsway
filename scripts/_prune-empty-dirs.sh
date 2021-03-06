#!/usr/bin/env sh
# -*- Mode: sh -*-
# _prune-empty-dirs.sh --- remove empty directories
# Copyright (C) 2017  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, June 13, 2017
# Version: 1.0
# Modified Time-stamp: <2017-11-16 09:23:12 dan.harms>
# Modified by: Dan Harms
# Keywords: utils helper directory

dir=${1:-$(pwd)}

if [ -d "$dir" ]; then
    find "$dir" -mindepth 1 -type d -empty -delete
fi

# code ends here
