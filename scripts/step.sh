#!/bin/bash
# -*- Mode: sh -*-
# step.sh --- debug stepping
# Copyright (C) 2020  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Wednesday, October  7, 2020
# Version: 1.0
# Modified Time-stamp: <2020-10-07 11:56:36 dharms>
# Modified by: Dan Harms
# Keywords: tools

trap '(read -p "[$BASH_SOURCE:$LINENO] $BASH_COMMAND?")' DEBUG

var=2
echo $((var+2))

# code ends here
