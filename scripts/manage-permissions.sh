#!/bin/sh
# -*- Mode: sh -*-
# manage-permissions.sh --- confirm permissions
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Monday, January  5, 2026
# Version: 1.0
# Modified Time-stamp: <2026-01-05 10:40:41 dharms>
# Modified by: Dan Harms
# Keywords:

find ~/.gnupg -type d -exec chmod 700 {} \;
find ~/.gnupg -type f -exec chmod 600 {} \;
find ~/.ssh -type d -exec chmod 700 {} \;
chmod 600 ~/.ssh/authorized_keys
chmod 644 ~/.ssh/*.pub

# code ends here
