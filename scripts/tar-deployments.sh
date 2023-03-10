#!/bin/sh
# -*- Mode: sh -*-
# tar-deployments.sh --- tar a set of repos for deployment
# Copyright (C) 2023  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, March 10, 2023
# Version: 1.0
# Modified Time-stamp: <2023-03-10 14:57:00 dharms>
# Modified by: Dan Harms
# Keywords:

archive=$( _realpath.sh "$1")
shift

for repo in "$@"; do
    abs=$( _realpath.sh "$repo" )
    manage-repo.sh "$abs"
    tar-deployment.sh "$abs" "$archive"
done

# code ends here
