#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# go to contracts folder
cd ./contracts
yarn add @openzeppelin/contracts@4.5.0

mkdir bin
wget https://github.com/ethereum/solc-bin/raw/gh-pages/linux-amd64/solc-linux-amd64-v0.8.0%2Bcommit.c7dfd78e -O bin/solc-0.8.0

# sha256sum -c solc.sha256

chmod +x bin/solc-0.8.0

ln -s ../../../node_modules/@openzeppelin .

find interfaces/ -name '*.sol' | while read i;do
  echo $i
  bin/solc-0.8.0 --allow-paths . $i
done

echo SUCCESS