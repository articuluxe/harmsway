#!/usr/bin/env bash
# -*- Mode: sh -*-
# _check_merge.sh --- merge word lists
# Copyright (C) 2017  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Friday, July  7, 2017
# Version: 1.0
# Modified Time-stamp: <2017-07-12 06:49:30 dharms>
# Modified by: Dan Harms
# Keywords: merge shell util
# Merges 2 files of words, orig and new, warning if
# new words need to be added to orig

origfile=$1
newfile=$2

if [ -z "$origfile" ]; then
    echo "!!! missing original file"
    exit 1
fi
if [ -z "$newfile" ]; then
    echo "!!! missing new file"
    exit 1
fi

merged=$(mktemp -t "merged-words-$USER.XXXXXXXX")
orig_sorted=$(mktemp -t "orig-words-sorted-$USER.XXXXXXXX")

cat "$origfile" "$newfile" | sort | uniq > "$merged"
sort < "$origfile" > "$orig_sorted"
output=$( diff "$orig_sorted" "$merged" )

newwords=$( sed -ne 's/^>\s*\(.*\)/\1/g p' << DOC
            $output
DOC
        )

# replace newlines in output with spaces
wordlist=${newwords//$'\n'/ }

if [ ! -z "$wordlist" ]; then
    echo "!!! $newfile has entries not in $origfile: $wordlist"
fi

# code ends here
