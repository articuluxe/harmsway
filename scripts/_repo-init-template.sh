#!/bin/bash
# -*- Mode: sh -*-
# _repo-init-template.sh --- init a src repo
# Copyright (C) 2016-2017  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, May  3, 2016
# Version: 1.0
# Modified Time-stamp: <2017-04-14 07:16:56 dharms>
# Modified by: Dan Harms
# Keywords: src repo

if [ $# -lt 2 ]; then
    echo "!!! Usage: repo-init.sh project version [target]"
    exit 1
fi

proj=$1
ver=$2
subdir=${3:-$proj-$ver}
echo "Using $proj version $ver"

_yesorno.sh "Clone $proj into $(pwd)/$subdir/proj?"
[ $? == 1 ] && exit

if [ ! -d $subdir/$proj/.git ]; then
    mkdir -p $subdir
    # git clone ssh://git@addr:7999/proj/proj.git $subdir/proj
fi

cd $subdir

setupf=$proj-setup.sh
prof=$proj-$ver.eprof
envf=$proj.env

cp -f ~/config/repos/$proj/$ver/$setupf repo-setup.sh
cp -f ~/config/repos/$proj/$ver/$prof .
cp -f ~/config/repos/$proj/$ver/$envf .repo-env

if [ ! -d $proj/.git/hooks ]; then
    echo "! $proj repo missing; install first."
    exit 1
fi

# code ends here
