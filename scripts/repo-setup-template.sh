#!/bin/bash
# -*- Mode: sh -*-
# repo-setup.sh --- set up repo
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, April 26, 2016
# Version: 1.0
# Modified Time-stamp: <2016-05-13 16:45:02 dan.harms>
# Modified by: Dan Harms
# Keywords: snap repo

cwd=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
REPO_ROOT=${REPO_ROOT:-$cwd}
envf=$REPO_ROOT/.repo-env

if [ -f $envf ]; then
   # export every non-empty line (VAR=VALUE)
   IFS=$'\n'; for e in $(cat $envf); do [ -z $e ] || eval export $e; done
else
   echo "! missing $envf; exiting..."
   exit 1
fi

# code ends here
