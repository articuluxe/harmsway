#!/usr/bin/env sh
# -*- Mode: sh -*-
# unarchive.sh --- unarchive a world archive
# Copyright (C) 2016  Dan Harms (dan.harms)
# Author: Dan Harms <dan.harms@xrtrading.com>
# Created: Tuesday, August  9, 2016
# Version: 1.0
# Modified Time-stamp: <2016-08-15 08:34:32 dharms>
# Modified by: Dan Harms
# Keywords: archive backup

tar=$TAR

if [ -z "$tar" ]; then
    tar=$(which tar)
    echo "Using $tar"
fi
if [ -z "$tar" ]; then
    echo "! no tar available; quitting"
    exit 1
fi

. _archive_utils.sh

if [ $# -ne 2 ]; then
    exe=$(basename $0)
    echo "Usage: $exe <source> <dest>"
    exit 1
fi

if [ ! -r "$1" ]; then
    echo "$1 is not readable; exiting..."
    exit 1
fi

dir=$1
if [ ! -d $dir ]; then
    echo "! invalid directory $dir; exiting..."
    exit 1
fi

cd $dir

stem=$(basename $dir)
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
    echo time is $tim
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

output=$2
mkdir -p $output
cd $output

echo going to restore from $start to $end inside $output

for ((j=$start; j <= $end; ++j)); do
    tar -G -xpzf "$dir/${filenames[$j]}"
done

# code ends here
