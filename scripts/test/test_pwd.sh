#!/bin/bash
# -*- Mode: sh -*-
# test_pwd.sh --- testing pwd
# Copyright (C) 2017-2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, October  3, 2017
# Version: 1.0
# Modified Time-stamp: <2018-01-04 16:57:07 dan.harms>
# Modified by: Dan Harms
# Keywords:

cwd=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
pwd="$PWD"

echo "Current is $cwd"
echo "Working is $pwd"

# code ends here
