# DAO Budgeting NFTs

Create a DAO budgeting system that is mediated by NFTs and Superfluid streams.

Add your own rpc URL for the kovan testnet and your own private keys for testing in your own `.env` file by copying `.env.template` .

1) Run ```yarn install``` to install dependencies.
2) Run ```yarn deploy:reset``` to deploy a new instance of the contract and run ```yarn verify``` to make Etherscan verify it.
3) Open up a stream to the contract using the Superfluid dashboard.
4) Run scripts to issue NFTs, edit NFTs, split streams, and merge streams. Make sure to input your own addresses as the sender & receiver.
```shell
yarn issue
yarn edit
yarn split
yarn merge
```

NOTE: This contract is unaudited and only manually tested. Please use for instructional purposes only. 


# Basic Sample Hardhat Project

This project demonstrates a basic Hardhat use case. It comes with a sample contract, a test for that contract, a sample script that deploys that contract, and an example of a task implementation, which simply lists the available accounts.

Try running some of the following tasks:

```shell
npx hardhat accounts
npx hardhat compile
npx hardhat clean
npx hardhat test
npx hardhat node
node scripts/sample-script.js
npx hardhat help
```
