#!/usr/bin/env bash

cd "$(dirname "$0")"/.. || exit 1

find contracts/{superfluid,agreements,gov,utils} -name '*.sol'  | while read -r i;do
    i=$(basename "$i")
    size=$(jq .bytecode build/contracts/"${i/.sol/.json}" | wc -c)
    size=$(( size / 2 ))
    echo "$i : $size ($(( 24576 - size )))"
done
