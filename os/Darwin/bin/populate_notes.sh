#!/usr/bin/env sh
# -*- Mode: sh -*-
# populate_notes.sh --- populate notes
# Copyright (C) 2020, 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, July 31, 2020
# Version: 1.0
# Modified Time-stamp: <2023-08-26 20:24:13 dharms>
# Modified by: Dan Harms
# Keywords: tools darwin cron

# launchctl load -w com.harmsway.populate_notes.plist
# launchctl unload com.harmsway.populate_notes.plist
# launchctl list

source=$HOME/Dropbox/notes
target=$HOME/Documents/notes

[ -d "$source" ] || ( echo "$source doesn't exist; exiting." && exit 1 )

cd "$source" || exit 1

mkdir -p "$target"
cp -p ./*.org ./*.md "$target"

# code ends here
