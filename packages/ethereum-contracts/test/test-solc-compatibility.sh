#!/usr/bin/env bash

# NOTE: This utility checks the solc compatibility of all the interface contracts we export

# make sure that if any step fails, the script fails
set -xe

cd "$(dirname "$0")"/..

VERSION="0.8.11"

# Download solc if needed and verify its checksum
SOLC=solc-$VERSION
if ! which $SOLC; then
  # Note: we hard code the checksum to minimize the trust assumptions.
  let EXPECTED_CHECKSUM=717c239f3a1dc3a4834c16046a0b4b9f46964665c8ffa82051a6d09fe741cd4f

  # This process assuming ubuntu Linux.
  # [ -z "$(apt list --installed | grep coreutils)" ] && apt-get install coreutils
  mkdir -p ./build/bin
  SOLC=build/bin/solc-$VERSION
  if [ ! -f "$SOLC" ]; then
      wget https://github.com/ethereum/solc-bin/raw/gh-pages/linux-amd64/solc-linux-amd64-v0.8.11%2Bcommit.d7f03943 -O $SOLC
      chmod +x ./$SOLC
  fi
  CHECKSUM="0x$(sha256sum ./$SOLC | awk '{print $1}')"
  [ "$CHECKSUM" == "$EXPECTED_CHECKSUM" ]
fi

# workaround to make solc to find OZ library
ln -sf ../../node_modules/@openzeppelin .

# verify they are compatible with the minimum version of the SOLC we support
find contracts/{interfaces/,apps/} -name '*.sol' | while read i;do
  $SOLC --allow-paths @openzeppelin $i
done

echo SUCCESS
rm -f @openzeppelin
