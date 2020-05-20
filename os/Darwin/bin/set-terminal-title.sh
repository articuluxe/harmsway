#!/bin/sh
# -*- Mode: sh -*-
# set-terminal-title.sh --- set terminal title on OS X
# Copyright (C) 2020  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Wednesday, May 20, 2020
# Version: 1.0
# Modified Time-stamp: <2020-05-20 11:34:08 dharms>
# Modified by: Dan Harms
# Keywords: tools

echo "\033]0;${1}\\007\c"

# code ends here
