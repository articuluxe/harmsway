#!/bin/sh
# -*- Mode: sh -*-
# repo-setup-template.sh --- set up repo
# Copyright (C) 2016, 2018, 2020, 2023  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, April 26, 2016
# Version: 1.0
# Modified Time-stamp: <2023-06-06 11:00:58 dharms>
# Modified by: Dan Harms
# Keywords: snap repo

cwd=$(dirname "$(realpath -q "${BASH_SOURCE[0]}")")
REPO_ROOT=${REPO_ROOT:-$cwd}
envf=$REPO_ROOT/.repo-env

[ -f "$envf" ] && . "$envf"

# code ends here
