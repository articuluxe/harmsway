#!/usr/bin/env sh
# -*- Mode: sh -*-
# _dirselect.sh --- select a dir from disk
# Copyright (C) 2016  Dan Harms (dharms)
# Author: Dan Harms <danielrharms@gmail.com>
# Created: Thursday, May  5, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-12 11:27:12 dan.harms>
# Modified by: Dan Harms
# Keywords: bash script

dir=${1:-$(pwd)}
prompt=${2:-"Select a file or directory: "}
# construct array of directories in dir
dirs=("$dir"/*)
# loop through, replace with basename
for e in "${dirs[@]}" ; do
    if [ -d $e ]; then
        dirsbase=( ${dirsbase[@]} $(basename $e) )
    fi
done
# concatenate into space-separated string
opts=$(echo "${dirsbase[*]}")

PS3="$prompt"
select opt in $opts "Quit"; do
    if [ "$opt" == "Quit" ]; then
        exit
    elif ((REPLY > 0)); then
        if [ ! -z $opt ]; then
            echo $opt
            exit
        fi
    fi
done

# code ends here
