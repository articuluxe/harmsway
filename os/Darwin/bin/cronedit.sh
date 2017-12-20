#!/usr/bin/env sh
# -*- Mode: sh -*-
# cronedit.sh --- edit cron on darwin
# Copyright (C) 2016-2017  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, August 18, 2016
# Version: 1.0
# Modified Time-stamp: <2017-12-20 09:13:16 dharms>
# Modified by: Dan Harms
# Keywords: cron

# usage: EDITOR=cronedit.sh crontab -e

$($VISUAL -nw "$@")

# code ends here
