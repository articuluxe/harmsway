#!/bin/bash
# -*- Mode: sh -*-
# secure_mount.sh --- mount secure volume
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, January  9, 2026
# Version: 1.0
# Modified Time-stamp: <2026-02-24 08:32:14 dharms>
# Modified by: Dan Harms
# Keywords: utilities

set -euo pipefail
set -x

VERACRYPT=$VERACRYPT_BIN/veracrypt
MOUNT=${SECURE_MOUNT:-~}
OS=$(uname -s)

if ! command -v "$VERACRYPT" > /dev/null ; then
    echo "veracrypt ($VERACRYPT) not installed or found; exiting."
    exit 1
fi

if [ "$OS" = "Darwin" ]; then
    pass App/veracrypt | \
    "$VERACRYPT" -t -k "" --stdin --non-interactive --protect-hidden=no --pim=0 \
                 ~/Sync/secure.volume \
                 "$MOUNT"/Secure
else
    pass App/veracrypt | \
    sudo "$VERACRYPT" -t -k "" --stdin --non-interactive --protect-hidden=no --pim=0 \
         ~/Sync/secure.volume \
         "$MOUNT"/Secure
fi

test -d "$MOUNT"/Secure && date > "$MOUNT"/Secure/.secure.last-mounted

# code ends here
