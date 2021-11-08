#!/bin/sh
# -*- Mode: sh -*-
# _realpath.sh --- canonicalize a relative path
# Copyright (C) 2021  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, November  5, 2021
# Version: 1.0
# Modified Time-stamp: <2021-11-05 12:38:37 dharms>
# Modified by: Dan Harms
# Keywords: sh
# cf. https://stackoverflow.com/questions/4175264/how-to-retrieve-absolute-path-given-relative/51264222#51264222

target="$1"
if [ "$target" = "." ]; then
    echo "$(pwd)"
elif [ "$target" = ".." ]; then
    echo "$(dirname $(pwd))"
else
    echo "$(cd $(dirname $1); pwd)/$(basename $1)"
fi

# code ends here
