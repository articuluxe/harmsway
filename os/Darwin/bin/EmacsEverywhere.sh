#!/bin/bash
/usr/local/bin/emacsclient -s "$(hostname -s)" --eval "(emacs-everywhere)"
# Automator: New -> Quick Action -> Run Shell Script
# Workflow receives no input
# Pass Input As Arguments
