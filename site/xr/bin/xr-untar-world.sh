#!/usr/bin/env bash
# -*- Mode: sh -*-
# xr-untar-world.sh --- untar important files
# Copyright (C) 2015-2018  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Monday, May 18, 2015
# Version: 1.0
# Modified Time-stamp: <2018-01-15 15:25:55 dan.harms>
# Modified by: Dan Harms
# Keywords: configuration

tar=$TAR
manifest=.bk_manifest
backup=emacs_bk.tar
os=$(uname)
host=$(hostname -s)
site=$SITE
input=
user_dict=~/.hunspell_en_US
logdir=.install-logs

function backup_file
{
    if [ -f ".emacs.d/$1" ] ; then
        echo Backing up "$1"
        $tar -rvf $backup ".emacs.d/$1"
    elif [ -d ".emacs.d/$1" ] ; then
        echo Backing up directory "$1"
        $tar -rvf $backup ".emacs.d/$1"
    fi
}

echo os is "$os"
echo host is "$host"
echo site is "$site"

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "! no tar available; quitting"
    exit 1
fi
if [ $# -gt 0 ] ; then
    input=$1
    shift
fi

if [ "x$input" = "x" ] ; then
    echo "Error: need an input file."
    exit
fi

date=$(date '+%F_%T' | tr ':' '-')

cd ~ || exit 1
mkdir -p "$logdir"

# there's an existing .emacs.d
$tar --overwrite -xpf "$input" .emacs.d/$manifest
if [ -d .emacs.d ] && [ -f .emacs.d/$manifest ] ; then
    rm -f $backup
    _prune-empty-dirs.sh .emacs.d/backups
    files=(`cat .emacs.d/$manifest`)
    numfiles=${#files[*]}
    i=0
    while [ $i -lt $numfiles ]
    do
        backup_file ${files[$i]}
        i=$(( $i+1 ))
    done
    # backup for posterity
    $tar czf "$logdir/.emacs.d.bk_$date.tgz" --force-local .emacs.d
    # restore interesting files
    rm -rf .emacs.d
    mkdir .emacs.d
    if [ -r $backup ] ; then
        $tar -xpf $backup
        rm -f $backup
    fi
fi

echo Unpacking "$input"
$tar --overwrite -xpf "$input"

# install user dictionary (warn if conflicts)
if [ -f "$user_dict" ]; then
    _check_merge.sh .emacs.d/etc/user-dict "$user_dict"
fi
cp .emacs.d/etc/user-dict "$user_dict"

# remove intermediate directories, if empty
for i in bash tcsh os/$os site/$site; do
    rmdir -p --ignore-fail-on-non-empty "$i"
done
# protect .netrc
[ -r .netrc ] && chmod 600 .netrc
# rename host files as part of distribution
[ -r .bash_host ] && mv .bash_host .bash_"$host"
[ -r .host.env ] && mv .host.env ."$host".env
# and byte-compile emacs
bin/emacscomp.sh .emacs.d

# untar-world.sh ends here
