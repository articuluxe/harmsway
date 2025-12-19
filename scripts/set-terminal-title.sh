#!/bin/bash
# -*- Mode: sh -*-
# set-terminal-title.sh --- set terminal title
# Copyright (C) 2020, 2025  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Wednesday, May 20, 2020
# Version: 1.0
# Modified Time-stamp: <2025-12-19 11:18:00 dharms>
# Modified by: Dan Harms
# Keywords: tools

# note: may not need final '\c'?
echo -ne "\033]0;${*}\007\c"

# code ends here
