#!/usr/bin/env bash

who=$1","
if [ -z "$1" ]
then
    who="you,"
fi

echo "One for $who one for me."
