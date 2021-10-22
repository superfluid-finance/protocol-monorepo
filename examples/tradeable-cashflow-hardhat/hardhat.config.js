require("@nomiclabs/hardhat-truffle5");
 require("@nomiclabs/hardhat-ethers");
require('hardhat-deploy');

 require("dotenv").config();
 const GAS_LIMIT = 8000000;

//  const defaultNetwork = 'kovan';
 
 module.exports = {
  // defaultNetwork,
  // networks: {
    // ganache: {
    //     url: "127.0.0.1",
    //     port: process.env.GANACHE_PORT || 8545,
    // },

    // kovan: {
    //   url: `${process.env.KOVAN_RPC_URL}`,
    //   accounts: [`0x${process.env.DEPLOYER_PRIVATE_KEY}`],
    //   gas: GAS_LIMIT,
    //   gasPrice: 11e9, // 10 GWEI
    //   confirmations: 6, // # of confs to wait between deployments. (default: 0)
    //   timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
    //   skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
    //   },
    // },
 
     solidity: {
      version: "0.8.13",
      settings: {
        optimizer: {
          enabled: true
        }
      }
    },
      namedAccounts: {
    deployer: 0
  }

  }
