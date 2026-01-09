#!/bin/sh
# -*- Mode: sh -*-
# mount_secure.sh --- mount secure volume
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, January  9, 2026
# Version: 1.0
# Modified Time-stamp: <2026-01-09 17:46:53 dharms>
# Modified by: Dan Harms
# Keywords: utilities

VERACRYPT=${VERACRYPT_BIN:-veracrypt}
MOUNT=${SECURE_MOUNT:-~}

if ! command -v "$VERACRYPT" > /dev/null ; then
    echo "veracrypt not installed or found; exiting."
    return 1
fi

pass App/veracrypt | \
    sudo \
    "$VERACRYPT" -t -k "" --stdin --non-interactive --protect-hidden=no --pim=0 \
               ~/Sync/secure.volume \
               "$MOUNT"/Secure

test -d "$MOUNT"/Secure && date > "$MOUNT"/Secure/.secure.last-mounted

# code ends here
