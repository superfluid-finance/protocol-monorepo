#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build contracts
EXAMPLES=(
    budget-nft
    continuous-auction
    flowlottery
    nftbillboard-userdata
    rewards-distribution-token
    sdk-redux-nextjs-typescript
    sdk-redux-react-typescript
    streaming-call-option
    tradeable-cashflow-hardhat
    tradeable-cashflow-truffle
)

for i in "${EXAMPLES[@]}";do
    cd $i
    yarn install
    chmod +x ./build-and-test.sh
    ./build-and-test.sh
    cd ..
done