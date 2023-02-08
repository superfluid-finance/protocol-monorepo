# Setup Guide

## Installations

- Set up StarkNet DevNet using this [guide](https://shard-labs.github.io/starknet-devnet/docs/intro)
- Set up protostar using this [guide](https://docs.swmansion.com/protostar/docs/tutorials/installation)

## Usage

- Build - `protostar build`
- Test - `protostar test`

## Deployment and Interaction (Local Environment)

In StarkNet, contracts are declared before deployment. Contract declarations returns a class hash(`CLASS_HASH`) which will be used for the deployment

- Declare Contract - `protostar -p devnet declare ./build/<CONTRACT_NAME>.json --account-address ACCOUNT_ADDRESS --max-fee FEE  --private-key-path ./.pkey`

- Deploy - `protostar -p devnet deploy CLASS_HASH --account-address ACCOUNT_ADDRESS --max-fee FEE  --private-key-path ./.pkey`

- `pkey` is a file containing the private key associated with `ACCOUNT_ADDRESS` gotten from the starknet devnet running starknet

- Use this [guide](https://docs.swmansion.com/protostar/docs/tutorials/interacting-with-starknet/invoke) for steps on invoking and calling contracts with protostar
