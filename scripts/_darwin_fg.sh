#!/bin/sh
# -*- Mode: sh -*-
# _darwin_fg.sh --- put window to front
# Copyright (C) 2025  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, April  1, 2025
# Version: 1.0
# Modified Time-stamp: <2025-04-01 11:05:19 dharms>
# Modified by: Dan Harms
# Keywords:

osascript <<EOF
tell application "System Events"
     tell application process "Emacs"
          set frontmost to true
     end tell
end tell
EOF

# code ends here
