#!/usr/bin/env sh
# -*- Mode: sh -*-
# archive_world.sh --- archive a host's vital files
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-22 08:18:15 dharms>
# Modified by: Dan Harms
# Keywords: archive backup
PATH=/bin:/sbin:~/bin

target=${1:-~/backups}
date=$( date '+%Y%m' )

_backup.sh ~/Dropbox/notes "$target-$date"

# code ends here
