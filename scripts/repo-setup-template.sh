#!/bin/sh
# -*- Mode: sh -*-
# repo-setup-template.sh --- set up repo
# Copyright (C) 2016, 2018, 2020  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, April 26, 2016
# Version: 1.0
# Modified Time-stamp: <2020-07-23 11:55:37 dharms>
# Modified by: Dan Harms
# Keywords: snap repo

cwd=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
REPO_ROOT=${REPO_ROOT:-$cwd}
envf=$REPO_ROOT/.repo-env

[ -f "$envf" ] && . "$envf"

# code ends here
