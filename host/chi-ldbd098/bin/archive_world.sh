#!/usr/bin/env sh
# -*- Mode: sh -*-
# archive_world.sh --- archive a host's vital files
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-22 08:17:47 dharms>
# Modified by: Dan Harms
# Keywords: archive backup

target=${1:-~/backups}
date=$( date '+%Y%m' )

_backup.sh ~/src/harmsway/site/xr "$target-$date"
_backup.sh ~/src/harmsway/host/chihq-dev-02d "$target-$date"
_backup.sh ~/src/harmsway/host/CHIHQ-DEV84 "$target-$date"
_backup.sh ~/src/harmsway/host/CHIHQ-IPC201 "$target-$date"

# code ends here
