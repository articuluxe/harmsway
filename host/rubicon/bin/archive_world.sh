#!/usr/bin/env sh
# -*- Mode: sh -*-
# archive_world.sh --- archive a host's vital files
# Copyright (C) 2016, 2017  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2017-01-04 06:20:36 dharms>
# Modified by: Dan Harms
# Keywords: archive backup

target=${1:-~/backups}
date=$( date '+%Y%m' )

_backup.sh ~/Dropbox/notes "$target-$date"
_backup.sh ~/Dropbox/secure "$target-$date"

# code ends here
