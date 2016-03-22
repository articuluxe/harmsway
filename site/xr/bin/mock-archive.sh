#!/usr/bin/env sh
# -*- Mode: sh -*-
# mock-archive.sh --- mock script archiver
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, March  1, 2016
# Version: 1.0
# Modified Time-stamp: <2016-03-22 11:30:05 dan.harms>
# Modified by: Dan Harms
# Keywords: mock scripting

archive=mock.tar

rm -f $archive

# snap, dependencies
tar -cf $archive xr-snap
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/libconfig/libconfig-1.4.8/lib libconfig++.so.9
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/libconfig/libconfig-1.4.8/lib libconfig++.so.9.1.2
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/stringencoders/3.10-1-custom/lib libmodpbase64.so
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/stringencoders/3.10-1-custom/lib libmodpbase64.so.0
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/stringencoders/3.10-1-custom/lib libmodpbase64.so.0.0.0
tar -rf $archive --directory=$SNAP_STATIC_TRUNK/xerces/3.1.1/xerces-c-3.1.1/lib libxerces-c-3.1.so
tar -rf $archive --directory=/usr/local/boost156/lib libboost_date_time.so.1.56.0
tar -rf $archive --directory=/usr/local/boost156/lib libboost_filesystem.so.1.56.0
tar -rf $archive --directory=/usr/local/boost156/lib libboost_iostreams.so.1.56.0
tar -rf $archive --directory=/usr/local/boost156/lib libboost_program_options.so.1.56.0
tar -rf $archive --directory=/usr/local/boost156/lib libboost_regex.so.1.56.0
tar -rf $archive --directory=/usr/local/boost156/lib libboost_system.so.1.56.0

# test files
(cd ../../../..; make mock-scripts)
tar -rf $archive mockobjects/Base.config
tar -rf $archive mockobjects/testscripts
tar -rf $archive mockrun.sh CMEmsgw.csv

gzip -f $archive

echo "Created $archive; saved $archive.gz"

# code ends here
