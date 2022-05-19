#!/bin/bash

# NOTE: This utility checks the solc-0.8.0 compatibility of all the interface contracts we export

# make sure that if any step fails, the script fails
set -xe

cd "$(dirname "$0")"/..

[ -z "$(apt list --installed | grep coreutils)" ] && apt-get install coreutils

# Download solc if needed and verify its checksum
mkdir -p ./build/bin
SOLC=build/bin/solc-0.8.2
if [ ! -f "$SOLC" ]; then
    wget https://github.com/ethereum/solc-bin/raw/gh-pages/linux-amd64/solc-linux-amd64-v0.8.2%2Bcommit.661d1103 -O $SOLC
    chmod +x ./$SOLC
fi
CHKSUM=$(sha256sum ./$SOLC | awk '{print $1}')
EXPECTED_CHKSUM="b6b9429d71d4395901795936a0aaee0b23082fcaee10d563d87b42e69c0e68c2"
[ "$CHKSUM" == "$EXPECTED_CHKSUM" ]

# https://github.com/ethereum/solc-bin/blob/gh-pages/linux-amd64/list.json
# 0x64016310a57caf1af76a3610f1f94c8848c04c9673e7fa268492e608918a4bdc
# TODO: get json file, find builds with path: solc-linux-amd64-v0.8.0%2Bcommit.c7dfd78e
# compare sha256 we generate with the downloaded bin file above with the sha256 obtained
# from here - don't forget to add 0x to our generated sha256

# workaround to make solc to find OZ library
ln -sf ../../node_modules/@openzeppelin .

# verify they are compatible with the minimum version of the SOLC we support
find contracts/interfaces/ -name '*.sol' | while read i;do
  echo $i
  $SOLC --allow-paths . $i
done

echo SUCCESS
rm -f @openzeppelin
