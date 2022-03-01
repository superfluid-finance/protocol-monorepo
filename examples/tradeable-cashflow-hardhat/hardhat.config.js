/**
 * @type import('hardhat/config').HardhatUserConfig
 */
 const HDWalletProvider = require("@truffle/hdwallet-provider");
 require("@nomiclabs/hardhat-truffle5");
require('hardhat-deploy');

 require("dotenv").config();
 const GAS_LIMIT = 8000000;


 
 module.exports = {
     networks: {
         ganache: {
             host: "127.0.0.1",
             network_id: "*",
             port: process.env.GANACHE_PORT || 8545,
         },

    kovan: {
      url: `${process.env.KOVAN_ALCHEMY_URL}`,
      gas: GAS_LIMIT,
      gasPrice:  1500000000,
      timeoutBlocks: 50,
      skipDryRun: false,
      accounts: [`0x${process.env.DEPLOYER_PRIVATE_KEY}`]
      },
    },
 
     solidity: {
      version: "^0.7.6",
      settings: {
        optimizer: {
          enabled: true
        }
      }
    }
  }
