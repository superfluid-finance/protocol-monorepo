#!/usr/bin/env bash

# NOTE: This utility checks the solc-0.8.4 compatibility of all the interface contracts we export

# make sure that if any step fails, the script fails
set -xe

cd "$(dirname "$0")"/..

# Download solc if needed and verify its checksum
SOLC=solc-0.8.4
if ! which $SOLC; then
  # This process assuming ubuntu Linux.
  [ -z "$(apt list --installed | grep coreutils)" ] && apt-get install coreutils
  mkdir -p ./build/bin
  SOLC=build/bin/solc-0.8.4
  if [ ! -f "$SOLC" ]; then
      wget https://github.com/ethereum/solc-bin/raw/gh-pages/linux-amd64/solc-linux-amd64-v0.8.4%2Bcommit.c7e474f2 -O $SOLC
      chmod +x ./$SOLC
  fi
  CHKSUM=$(sha256sum ./$SOLC | awk '{print $1}')
  EXPECTED_CHKSUM="f7115ccaf11899dcf3aaa888949f8614421f2d10af65a74870bcfd67010da7f8"
  [ "$CHKSUM" == "$EXPECTED_CHKSUM" ]
fi

# https://github.com/ethereum/solc-bin/blob/gh-pages/linux-amd64/list.json
# 0x64016310a57caf1af76a3610f1f94c8848c04c9673e7fa268492e608918a4bdc
# TODO: get json file, find builds with path: solc-linux-amd64-v0.8.4%2Bcommit.c7e474f2
# compare sha256 we generate with the downloaded bin file above with the sha256 obtained
# from here - don't forget to add 0x to our generated sha256

# workaround to make solc to find OZ library
ln -sf ../../node_modules/@openzeppelin .

# verify they are compatible with the minimum version of the SOLC we support
find contracts/{interfaces/,apps/} -name '*.sol' | while read i;do
  # TODO: these use abi.encodeAbi, which is only available from 0.8.11
  [ "$i" == contracts/apps/SuperAppBaseFlow.sol ] && continue;
  [ "$i" == contracts/apps/SuperTokenV1Library.sol ] && continue;
  [ "$i" == contracts/apps/CFAv1Library.sol ] && continue;
  [ "$i" == contracts/apps/IDAv1Library.sol ] && continue;

  $SOLC --allow-paths . $i
done

echo SUCCESS
rm -f @openzeppelin
