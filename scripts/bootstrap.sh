#!/bin/sh
# -*- Mode: sh -*-
# bootstrap.sh --- bootstrap the world
# Copyright (C) 2021  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Wednesday, February 24, 2021
# Version: 1.0
# Modified Time-stamp: <2021-03-01 13:31:58 dharms>
# Modified by: Dan Harms
# Keywords: tools

cd ~
mkdir -p src
cd src
git clone --recurse-submodules https://github.com/articuluxe/harmsway.git
git clone https://github.com/articuluxe/gridlock.git
git clone https://github.com/articuluxe/ncio.git
git clone https://github.com/articuluxe/outrespace.git
git clone https://github.com/articuluxe/parsenv.git
git clone https://github.com/articuluxe/proviso.git
git clone https://github.com/articuluxe/punch.git
git clone https://github.com/articuluxe/xfer.git
git clone https://bailiwick@bitbucket.org/bailiwick/intervista.git

# code ends here
