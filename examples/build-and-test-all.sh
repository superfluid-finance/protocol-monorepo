#!/bin/bash

# in order to use this script, you must ensure that your lockfiles are up to date
# you can do this by removing and reinstalling the lockfiles

# make sure that if any step fails, the script fails
set -xe

# build contracts
EXAMPLES=(
    beginner-examples/money-router
    beginner-examples/token-spreader
    budget-nft
    employment-based-loan
    nftbillboard-userdata
    rewards-distribution-token
    sdk-redux-examples/sdk-redux-nextjs-typescript
    sdk-redux-examples/sdk-redux-react-typescript
    simple-acl-close
    stream-in-distribute-out
    tradeable-cashflow
)

for i in "${EXAMPLES[@]}";do
    cd $i
    pnpm install
    cd ~/Documents/dev/superfluid/protocol-monorepo/examples/
done
