# Superfluid NFT Billboard 

Learn how to insert and make use of additional metadata within your super agreements.

In this tutorial, you'll use Superfluid UserData to pay to post a message on an NFT billboard. We'll use scaffold-eth, hardhat, and react to get the job done ⛏

Note: this code has not been audited and is for example purposes only. 

We're excited to see what you build 💻

### Usage

1) Go get some test DAIx tokens at app.superfluid.finance on the test network of your choice.
2) Add your own private key, public address and rpc URL for your network of choice inside of a ```.env``` file using the format in ```packages/hardhat/env.example```
3) Deploy Your Billboard contract by following the instructions below.
4) Add your own custom message inside of each of the scripts as the value that is encoded and set to the ```userData``` variable.

### For Initial Setup and Contract Deployment:

- You need to change the `defaultNetwork` inside of `hardhat.config.js` to reflect your target network.
- If you're deploying locally, `ganache` or `localhost` should suffice. 
- If you're deploying to a live mainnet or testnet, you should change the default network and make sure that your private key and rpc url for that network are correct in your `.env` file.
- Inside of the deploy script, you will also need to make sure that the deployment is using the super token you're intending to use for deployment. The default is `fDAIx`. See line 51 and line 69 of the 00_deploy_tradeable_cashflow.js script for where you need to update these params. The `host` should be correct each time because we're getting the host address from the `Framework` object when deploying the [Superfluid SDK Core](https://docs.superfluid.finance/superfluid/developers/sdk-initialization/sdk-core/sdk-core-initialization).

Once the above is complete, you can run:

```yarn install``` to install dependencies
```yarn start``` to start the react app
```yarn deploy``` to deploy the tradable cashflow contract

To create a flow and set a message on the billboard, you can go to the ```createFlow()``` script and pass in your own message to the ```message``` variable at the top of the file. Then run:
```yarn createFlow```

To update a flow and change the message on the billboard, you can go to the ```updateFlow()``` script and pass in your own message to the ```newMessage``` variable at the top of the file. Then run:
```yarn updateFlow```

To delete flows (and the message)
```yarn deleteFlow```

To read data and see userData logged in the console:
```yarn readData```


NOTE: be careful with your private keys! Do not publish them to github. 

This project is a fork of [Scaffold ETH](https://docs.scaffoldeth.io/scaffold-eth/). Huge props to [Austin Griffith](https://twitter.com/austingriffith) 🙌🏻 We are grateful for what he's done for the Ethereum ecosystem.

# 🏗 Scaffold-ETH

> everything you need to build on Ethereum! 🚀

🧪 Quickly experiment with Solidity using a frontend that adapts to your smart contract:

![image](https://user-images.githubusercontent.com/2653167/124158108-c14ca380-da56-11eb-967e-69cde37ca8eb.png)

