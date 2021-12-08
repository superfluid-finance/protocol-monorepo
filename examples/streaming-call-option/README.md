# Streaming Call Option

**Use Superfluid Streams, Price Oracles, and NFTs to Create an Options Contract**

Disclaimer - this code is unaudited and should not be used by itself in production. It is meant to be a proof of concept & used for instructional purposes. 

**Basic Overview**
1) Run npm install
2) Run hardhat deploy --reset
3) Use scripts and the Superfluid dashboard to interact with the contract

**Steps to deploy and interact with your option:**
1) Run hardhat deploy --reset to deploy the option
2) Run the createOption.js script by calling npx hardhat run scripts/createOption.js - pass in your own parameters!
3) Approve the option contract to spend the underlying asset specified inside of the call option script - this can be done via Etherscan
4) Create a flow into the contract with a flowrate >= the requiredFlowRate specified in createOption.js - this activates the option (do this from a separate account from the one that called createOption.js)
5) Approve the contract to spend your DAI at an amount that is >= the strike price of the option contract
6) Call exerciseOption() using the exerciseOption.js script. Run this script using the same account that created the flow into the contract. This function will settle the option if all parameters are met (option is in the money and block.timestamp <= expirationDate).


**NOTE: All scripts use Rinkeby. To use them, change the network to Rinkeby in hardhat.config.**

If you'd like to change this, simply make adjustments to hardhat config & the scripts folder. 
You will also need to change the addresses of each Superfluid contract & token contract you're interacting with. 
You can make these changes in the deploy script in the deploy folder and in each script.


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
