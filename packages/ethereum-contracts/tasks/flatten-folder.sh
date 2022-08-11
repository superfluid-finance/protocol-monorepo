#!/bin/bash
# https://unix.stackexchange.com/questions/52814/flattening-a-nested-directory

cd "$(dirname "$0")"/..

path=$1
not_name=$2
dest=$3

# flattens folders of $path for files that do not have name $not_name and moves them to $dest
find $path ! -name $not_name -mindepth 2 -type f -exec mv -f '{}' $dest ';'