#!/bin/bash
# -*- Mode: sh -*-
# PrintMacOs.sh --- print on Darwin
# Copyright (C) 2026  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Thursday, July 16, 2026
# Version: 1.0
# Modified Time-stamp: <2026-07-16 11:02:01 dharms>
# Modified by: Dan Harms
# Keywords: print utility

# need to install gs: brew install ghostscript

uuid=$(uuidgen)
tmpfile="${TMPDIR}/${uuid}".pdf

gs -sDEVICE=pdfwrite \
   -sPAPERSIZE=letter \
   -dFIXEDMEDIA \
   -dPDFFitPage \
   -dCompatibilityLevel=1.4 \
   -o "$tmpfile" - \
   && open "$tmpfile" \
   && sleep 1 \
   && rm "$tmpfile"

# code ends here
