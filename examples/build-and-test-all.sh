#!/bin/bash

# in order to use this script, you must ensure that your lockfiles are up to date
# you can do this by removing and reinstalling the lockfiles

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
    chmod +x ./build-and-test.sh
    ./build-and-test.sh
    cd ..
done
