#!/usr/bin/env sh
# -*- Mode: sh -*-
# test_tar.sh --- test tar archiving
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-20 08:01:32 dharms>
# Modified by: Dan Harms
# Keywords: tar

base=${1:-"basedir"}
output=${2:-"output"}

tar=$TAR

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "!!! no tar available; quitting"
    exit 1
fi

echo "setting up $base"

mkdir -p $base

# stage 1
cd $base
echo "This is written in stage 1" >> SinceStage1
echo "This should be removed in stage 3" >> toRemoveIn3
cd ..

$tar -g test.snar -cpf test_0.tar $base
cp test.snar test0.snar
$tar -G -tvvpf test_0.tar

sleep 1                         # needed on darwin

# stage 2
cd $base
echo " and updated in stage 2" >> SinceStage1
echo "This is created in stage 2" >> SinceStage2
cd ..

$tar -g test.snar -cpf test_1.tar $base
cp test.snar test1.snar
$tar -G -tvvpf test_1.tar

sleep 1                         # needed on darwin

# stage 3
cd $base
echo " and finally updated in stage 3" >> SinceStage1
echo " and updated in stage 3" >> SinceStage2
rm toRemoveIn3
cd ..

$tar -g test.snar -cpf test_2.tar $base
cp test.snar test2.snar
$tar -G -tvvpf test_2.tar

# extract
mkdir -p $output
cp test.snar test_*.tar $output
cd $output
$tar -G -xpf test_0.tar
$tar -G -xpf test_1.tar
$tar -G -xpf test_2.tar

echo "SinceStage1:"
cat $base/SinceStage1
echo "SinceStage2:"
cat $base/SinceStage2

# code ends here
