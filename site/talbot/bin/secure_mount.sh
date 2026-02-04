#!/bin/bash
# -*- Mode: sh -*-
# secure_mount.sh --- mount secure volume
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, January  9, 2026
# Version: 1.0
# Modified Time-stamp: <2026-02-03 17:08:20 dharms>
# Modified by: Dan Harms
# Keywords: utilities

set -euo pipefail
# set -x

VERACRYPT=${VERACRYPT_BIN/veracrypt:-veracrypt}
MOUNT=${SECURE_MOUNT:-~}
OS=$(uname -s)
SUDO="sudo"

if ! command -v "$VERACRYPT" > /dev/null ; then
    echo "veracrypt not installed or found; exiting."
    return 1
fi

if [ "$OS" = "Darwin" ]; then
    SUDO=""
fi

pass App/veracrypt | \
    "$SUDO" \
    "$VERACRYPT" -t -k "" --stdin --non-interactive --protect-hidden=no --pim=0 \
               ~/Sync/secure.volume \
               "$MOUNT"/Secure

test -d "$MOUNT"/Secure && date > "$MOUNT"/Secure/.secure.last-mounted

# code ends here
