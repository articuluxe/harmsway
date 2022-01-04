#!/bin/sh
# -*- Mode: sh -*-
# bootstrap-harmsway.sh --- bootstrap harmsway
# Copyright (C) 2021-2022  Dan Harms (dharms)
# Author: Dan Harms <enniomore@icloud.com>
# Created: Wednesday, March 10, 2021
# Version: 1.0
# Modified Time-stamp: <2022-01-04 11:22:44 dharms>
# Modified by: Dan Harms
# Keywords: tools

OS=$(uname)

if [ "$OS" = "Darwin" ]; then
    brew install gnu-tar
    TAR=$(which gtar)
else
    TAR=$(which tar)
fi

mkdir -p "$HOME/src"
cd "$HOME/src" || exit 1
test -d harmsway/.git || git clone --recurse-submodules git@github.com:articuluxe/harmsway.git
cd harmsway || exit 1

rm -f world.tar
$TAR cf world.tar config doc src .fonts .config .terminfo .proviso.d
$TAR uf world.tar --exclude=*.elc .emacs.d
$TAR uf world.tar --transform=s%ext%.emacs.d/ext% .ext
$TAR uf world.tar --transform=s/scripts/bin/ scripts
$TAR uf world.tar --transform=s/dotfiles\\/// dotfiles
$TAR uf world.tar --transform=s/bash\\/// bash
$TAR uf world.tar --transform=s%os/$OS\\/%% os/$OS

mv world.tar ~
cd ~ || exit 1
$TAR xf world.tar

# code ends here
