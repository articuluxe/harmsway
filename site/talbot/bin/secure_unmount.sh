#!/bin/bash
# -*- Mode: sh -*-
# secure_unmount.sh --- unmount secure volume
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, January  9, 2026
# Version: 1.0
# Modified Time-stamp: <2026-02-03 17:08:20 dharms>
# Modified by: Dan Harms
# Keywords: utilities

set -euo pipefail
# set -x

VERACRYPT=${VERACRYPT_BIN:-veracrypt}
MOUNT=${SECURE_MOUNT:-~}

if ! command -v "$VERACRYPT" > /dev/null ; then
    echo "veracrypt not installed or found; exiting."
    return 1
fi

if [ -d "$MOUNT"/Secure ]; then

    date > "$MOUNT"/Secure/.secure.last-unmounted

    "$VERACRYPT" -t -d ~/Sync/secure.volume

fi

# code ends here
