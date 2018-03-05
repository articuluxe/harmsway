#!/bin/bash
# -*- Mode: sh -*-
# repo-setup-template.sh --- set up repo
# Copyright (C) 2016, 2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, April 26, 2016
# Version: 1.0
# Modified Time-stamp: <2018-03-05 13:30:23 dan.harms>
# Modified by: Dan Harms
# Keywords: snap repo

cwd=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
REPO_ROOT=${REPO_ROOT:-$cwd}
envf=$REPO_ROOT/.repo-env

[ -f "$envf" ] && . "$envf"

# code ends here
