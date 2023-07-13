# Setup Guide

## Installations

- Set up StarkNet DevNet using this [guide](https://0xspaceshard.github.io/starknet-devnet/docs/intro)
- Set up protostar using this [guide](https://docs.swmansion.com/protostar/docs/legacy/installation)

## Usage

- Build - `protostar build-cairo0`
- Test - `protostar test-cairo0`

## Demo

- [Live Dapp](https://starknet-superfluidv2-demo.vercel.app/)
- [Contract](https://testnet.starkscan.co/contract/0x0517ce183b644b9f7484c498c7e9071542c724f999b9d815e1f9e5cc4c8617aa#read-write-contract)
- [Faucet](https://faucet.goerli.starknet.io/)

## Deployment and Interaction (Local Environment)

In StarkNet, contracts are declared before deployment. Contract declarations returns a class hash(`CLASS_HASH`) which will be used for the deployment

- Declare Contract - `protostar -p devnet declare-cairo0 ./build/<CONTRACT_NAME>.json --account-address ACCOUNT_ADDRESS --max-fee FEE  --private-key-path ./.pkey`

- Deploy - `protostar -p devnet deploy CLASS_HASH --account-address ACCOUNT_ADDRESS --max-fee FEE  --private-key-path ./.pkey`

- `pkey` is a file containing the private key associated with `ACCOUNT_ADDRESS` gotten from the starknet devnet running starknet

- Use this [guide](https://docs.swmansion.com/protostar/docs/legacy/interacting-with-starknet) for steps on invoking and calling contracts with protostar
