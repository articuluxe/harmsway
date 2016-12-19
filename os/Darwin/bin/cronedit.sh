#!/usr/bin/env sh
# -*- Mode: sh -*-
# cronedit.sh --- edit cron on darwin
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, August 18, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-18 06:36:55 dharms>
# Modified by: Dan Harms
# Keywords: cron

# usage: EDITOR=cronedit.sh crontab -e

$($EMACSX -nw "$@")

# code ends here
