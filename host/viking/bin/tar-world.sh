#!/bin/bash
# -*- Mode: sh -*-
# tar-world.sh --- tar up important configuration files
# Copyright (C) 2015-2019, 2023  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Friday, May 29, 2015
# Version: 1.0
# Modified Time-stamp: <2023-09-07 11:14:26 dharms>
# Modified by: Dan Harms
# Keywords: configuration

tar=$TAR
user=$(id -nu)
os=$(uname)
host=$(hostname -s)
site="$SITE"
dest=world.tar
verbose=

if [ -z "$tar" ]; then
   tar=$(command -v tar)
   echo "Using $tar"
fi
if [ -z "$tar" ]; then
   echo "! no tar available; quitting"
   exit 1
fi
if [ $# -gt 0 ] ; then
   dest=$1
   shift
fi

echo user is "$user"
echo os is "$os"
echo host is "$host"
echo site is "$site"

echo Generating "$dest"...

if [ -f "$dest" ] ; then
   rm -f "$dest"
fi

$tar c"$verbose"f "$dest" config doc src .gnupg .fonts .config .terminfo .proviso.d
$tar u"$verbose"f "$dest" --exclude=*.elc .emacs.d
$tar u"$verbose"f "$dest" --transform=s%ext%.emacs.d/ext% ext
$tar u"$verbose"f "$dest" --transform=s/scripts/bin/ scripts
$tar u"$verbose"f "$dest" --transform=s/dotfiles\\/// dotfiles
$tar u"$verbose"f "$dest" --transform=s/bash\\/// bash
$tar u"$verbose"f "$dest" --transform=s/tcsh\\/// tcsh
$tar u"$verbose"f "$dest" --transform=s%user/"$user"\\/%% user/"$user"
$tar u"$verbose"f "$dest" --transform=s%os/"$os"\\/%% os/"$os"
$tar u"$verbose"f "$dest" --transform=s%host/"$host"\\/%% host/"$host"
$tar u"$verbose"f "$dest" --transform=s%site/"$site"\\/%% site/"$site"

deploys=(
    repos/cpp/tree-sitter
    repos/cpp/universal-ctags
    repos/rust/ripgrep
    repos/rust/fd
    src/samples/time_t_decoder
)
tar-deployments.sh "$dest" "${deploys[@]}"

echo ...done generating "$dest"

# tar-world.sh ends here
