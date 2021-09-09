#!/bin/bash
cwd=$(dirname "$1")
osascript -e 'tell application "Emacs" to activate'
/usr/local/bin/emacsclient -q -s rubicon -n -c --eval "(progn (dired \"$cwd\") (dired-goto-file \"$1\"))"

# Automator: New -> Quick Action -> Run Shell Script
# Workflow receives Current Files or Folders in Finder
# Image: Share
# Pass Input as arguments
