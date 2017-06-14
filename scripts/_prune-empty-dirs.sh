#!/usr/bin/env sh
# -*- Mode: sh -*-
# _prune-empty-dirs.sh --- remove empty directories
# Copyright (C) 2017  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, June 13, 2017
# Version: 1.0
# Modified Time-stamp: <2017-06-13 08:50:21 dharms>
# Modified by: Dan Harms
# Keywords: utils helper directory

dir=${1:-$(pwd)}

find "$dir" -type d -mindepth 1 -empty -delete

# code ends here
