// require("@nomiclabs/hardhat-waffle");
require("@nomiclabs/hardhat-truffle5");
require('hardhat-deploy');
require("hardhat/config").HardhatUserConfig
require("dotenv").config();

// This is a sample Hardhat task. To learn how to create your own go to
// https://hardhat.org/guides/create-task.html
task("accounts", "Prints the list of accounts", async (taskArgs, hre) => {
  const accounts = await hre.ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});

const defaultNetwork = "goerli";
// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  defaultNetwork,

  solidity: {
    version: "0.8.13",
    settings: {
      optimizer: {
        enabled: true
      }
    }
  },

  networks: {
    // ganache: {
    //   url: "http://127.0.0.1:8545",
    //   chain_id: "1337",
    //   port: process.env.GANACHE_PORT || 8545,
    //   // accounts: {
    //   //   mnemonic: `${process.env.GOERLI_MNEMONIC}`
    //   // }
    // },
    goerli: {
      url: `${process.env.GOERLI_URL}`,
      accounts: [`${process.env.PRIVATE_KEY}`]
      },
    },
  
    namedAccounts: {
      deployer: {
        default: 0, // here this will by default take the first account as deployer
      },
  }
}