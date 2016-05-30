#!/usr/bin/env sh
# -*- Mode: sh -*-
# ctags-install.sh --- install ctags distribution
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 30, 2016
# Version: 1.0
# Modified Time-stamp: <2016-05-30 11:20:26 dan.harms>
# Modified by: Dan Harms
# Keywords: ctags install

bundle=ctags.bundle
#CC=/usr/bin/gcc

pushd ~/src
if [ ! -f "$bundle" ]; then
   echo "Missing $bundle; exiting..."
   exit 1
fi

if [ -d ./ctags ]; then
   rm -rf ./ctags
fi
git clone $bundle -b master ctags

cd ctags
usepackage -b x3p-dev
autogen.sh
mkdir -p bld
cd bld
#CC=$CXX
../configure --prefix=$HOME --program-prefix=ex &&
   make &&
   make install

popd

# code ends here
