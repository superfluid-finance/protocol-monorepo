#!/bin/bash

# in order to use this script, you must ensure that your lockfiles are up to date
# you can do this by removing and reinstalling the lockfiles

# make sure that if any step fails, the script fails
set -xe

# build contracts
EXAMPLES=(
    budget-nft
    beginner-examples/money-router
    beginner-examples/token-spreader
    nftbillboard-userdata
    employment-based-loan
    simple-acl-close
    stream-in-distribute-out
    rewards-distribution-token
    sdk-redux-examples/sdk-redux-nextjs-typescript
    sdk-redux-examples/sdk-redux-react-typescript
    tradeable-cashflow
)

for i in "${EXAMPLES[@]}";do
    cd $i
    chmod +x ./build-and-test.sh
    ./build-and-test.sh
    cd ..
done
