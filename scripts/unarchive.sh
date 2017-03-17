#!/bin/bash
# -*- Mode: sh -*-
# unarchive.sh --- unarchive a world archive
# Copyright (C) 2016-2017  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2017-03-16 17:39:12 dharms>
# Modified by: Dan Harms
# Keywords: archive backup

tar=$TAR

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "!!! no tar available; quitting"
    exit 1
fi

. _archive_utils.sh

if [ $# -ne 2 ]; then
    exe=$(basename "$0")
    echo "Usage: $exe <source> <dest>"
    exit 1
fi

dir=$1
# require readable source in current dir, or absolute path
is_absolute_path "$dir"
if [ $? == 1 ]; then
    dir="$PWD/$dir"
fi
if [ ! -d "$dir" ]; then
    echo "!!! invalid directory $dir; exiting..."
    exit 1
fi
if [ ! -r "$dir" ]; then
    echo "!!! $dir is not readable; exiting..."
    exit 1
fi

output=$2
is_absolute_path "$output"
if [ $? == 1 ]; then
    output="$PWD/$output"
fi
if [ -r "$output" ]; then
    echo "!!! output directory $output already exists; exiting..."
    exit 1
fi

cd "$dir"

stem=$(basename "$dir")
stemlen=${#stem}
shopt -s nullglob
files=( ${stem}_*.tar.gz )
shopt -u nullglob
max=${#files[@]}

if ((max == 0)); then
    echo "! no archive present; exiting..."
    exit 1
fi

i=0
for file in ${files[@]}; do
    filenames[i]=$file
    tim=$(stat -c "%y" $file)
    filetimes[i]=$tim
    i=$(($i+1))
done
max=i
start=0
end=$((i-1))

OIFS=$IFS
IFS=,
PS3="Select the time of the archive to restore: "
select opt in "Latest" "${filetimes[@]}" "Quit"; do
    if [ "$opt" == "Quit" ]; then
        IFS=$OIFS
        exit 0
    elif [ "$opt" == "Latest" ]; then
        break
    elif ((REPLY > 0)); then
        if [ ! -z "$opt" ]; then
            echo Selected "$opt", size ${#filetimes[@]}
            for ((j=0; j < max; ++j)); do
                if [ "${filetimes[$j]}" = "$opt" ]; then
                    end=$j
                    break 2
                fi
            done
            echo "! invalid selection; exiting..."
            IFS=$OIFS
            exit 1
        fi
    fi
done

IFS=$OIFS

mkdir -p $output
cd $output

echo Restoring levels from $start to $end inside $output

for ((j=$start; j <= $end; ++j)); do
    tar -G -xpzf "$dir/${filenames[$j]}"
done

# code ends here
