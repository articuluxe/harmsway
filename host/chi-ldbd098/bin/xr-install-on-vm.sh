#!/usr/bin/env sh
# -*- Mode: sh -*-
# xr-install-on-vm.sh --- install xr-world on vm
# Copyright (C) 2018-2019  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Thursday, March  1, 2018
# Version: 1.0
# Modified Time-stamp: <2019-08-28 10:24:22 dan.harms>
# Modified by: Dan Harms
# Keywords: xr config

cd ~/src/harmsway/ || exit 1
xr-tar-world.sh
ssh vm 'xr-install-world.sh'

# code ends here
