#!/bin/bash
# -*- Mode: sh -*-
# _yesorno.sh --- ask user a yes or no question
# Copyright (C) 2016-2017  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Wednesday, June  8, 2016
# Version: 1.0
# Modified Time-stamp: <2017-04-14 07:17:09 dharms>
# Modified by: Dan Harms
# Keywords: scripting

# call like this:
# _yesorno.sh
# [ $? == 1 ] && exit
# #User selected yes

prompt=${1:-"Are you sure?"}

read -p "$prompt [y/n] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
   exit 0
fi

exit 1

# code ends here
