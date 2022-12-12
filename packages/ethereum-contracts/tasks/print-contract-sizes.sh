#!/bin/bash

cd "$(dirname "$0")"/..

find contracts/{superfluid,agreements,gov,utils} -name '*.sol'  | while read i;do
    i=$(basename $i)
    size=$(jq .bytecode build/contracts/${i/.sol/.json} | wc -c)
    size=$(( $size / 2 ))
    echo "$i : $size ($(( 24576 -$size )))"
done
