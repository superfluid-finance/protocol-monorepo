require("@nomiclabs/hardhat-waffle");
require("dotenv").config();
require("hardhat-deploy");

// This is a sample Hardhat task. To learn how to create your own go to
// https://hardhat.org/guides/create-task.html
task("accounts", "Prints the list of accounts", async (taskArgs, hre) => {
  const accounts = await hre.ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});

// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

/**
 * @type import('hardhat/config').HardhatUserConfig
 */

 const defaultNetwork = "kovan";


//fix compiler
//let's set up a local testnet this time
//fork mainnet with archive node
module.exports = {
  defaultNetwork,
  solidity: {
    version: "0.8.0",
    settings: {
      optimizer: {
        enabled: true
      }
    }
  },
  networks: {
    kovan: {
      url: `${process.env.ALCHEMY_URL}`,
      gasPrice:  1700000000,
      accounts: [`${process.env.PK}`]
      },
  },
  etherscan: {
    // Your API key for Etherscan
    // Obtain one at https://etherscan.io/
    apiKey: process.env.ETHERSCAN_API_KEY
  },
  namedAccounts: {
    deployer: {
      default: 0, // here this will by default take the first account as deployer
    },
  }
};



