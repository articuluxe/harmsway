#!/usr/bin/env sh
# -*- Mode: sh -*-
# archive_world.sh --- archive a host's vital files
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-17 17:15:52 dharms>
# Modified by: Dan Harms
# Keywords: archive backup

target=${1:-~/backups}

_backup.sh ~/Dropbox/notes "$target"

# code ends here
