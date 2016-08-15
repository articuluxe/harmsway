#!/usr/bin/env sh
# -*- Mode: sh -*-
# _archive_utils.sh --- utils for archive/unarchive
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Wednesday, August 10, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-10 17:28:21 dharms>
# Modified by: Dan Harms
# Keywords: backup archive

function sanitize {
    clean=$1
    # remove any leading and trailing slashes
    clean=$(echo $clean | sed -e 's$^/$$')
    clean=$(echo $clean | sed -e 's%/$%%')
    # replace slashes with dots
    clean=${clean////.}
    # replace underscores with dots
    clean=${clean//_/.}
    # allow only alphanumeric + .
    clean=${clean//[^a-zA-Z0-9]/.}
    # collapse consecutive dots
    clean=$(echo $clean | tr -s '\0-\255')
    echo "$clean"
}

# code ends here
