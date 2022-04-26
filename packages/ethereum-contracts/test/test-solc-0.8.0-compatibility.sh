#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

cd "$(dirname "$0")"/..

[ -z "$(apt list --installed | grep coreutils)" ] && apt-get install coreutils

mkdir -p ./build/bin
SOLC=build/bin/solc-0.8.0
if [ ! -f "$SOLC" ]; then
    wget https://github.com/ethereum/solc-bin/raw/gh-pages/linux-amd64/solc-linux-amd64-v0.8.0%2Bcommit.c7dfd78e -O $SOLC
    chmod +x ./$SOLC
fi
sha256sum ./$SOLC

# https://github.com/ethereum/solc-bin/blob/gh-pages/linux-amd64/list.json
# 0x64016310a57caf1af76a3610f1f94c8848c04c9673e7fa268492e608918a4bdc
# TODO: get json file, find builds with path: solc-linux-amd64-v0.8.0%2Bcommit.c7dfd78e
# compare sha256 we genrate with the downloaded bin file above with the sha256 obtained
# from here - don't forget to add 0x to our generated sha256

ln -sf ../../node_modules/@openzeppelin .

find contracts/interfaces/ -name '*.sol' | while read i;do
  echo $i
  $SOLC --allow-paths . $i
done

echo SUCCESS
rm -f @openzeppelin
