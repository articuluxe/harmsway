#!/bin/bash
osascript -e 'tell application "Emacs" to activate'
/usr/local/bin/emacsclient -q -s "$(hostname -s)" -n -c --eval "(find-file \"$1\")"

# Automator: New -> Quick Action -> Run Shell Script
# Workflow receives Current Files or Folders in Finder
# Image: Share
# Pass Input as arguments
