#!/bin/env sh



OPTIONS=$(getopt -o hf:gb -l help, file:,foo,bar -- "$@")

if [ $? -ne 0 ]; then
   echo "getopt error"
   exit 1
fi

eval set -- $OPTIONS

while true; do
   case "$1" in
      -h|--help) HELP=1 ;;
      -f|--file) FILE="$2" ; shift ;;
      -g|--foo)  FOO=1 ;;
      -b|--bar)  BAR=1 ;;
      --)        shift ; break ;;
      *)         echo "unknown option: $1" ; exit 1 ;;
   esac
   shift
done

if [ $# -ne 0 ]; then
   echo "unknown option(s): " "$@"
   exit 1
fi

echo "help: $HELP"
echo "file: $FILE"
echo "foo: $FOO"
echo "bar: $BAR"
