require("@nomiclabs/hardhat-waffle");
require("@nomiclabs/hardhat-truffle5");
require("@nomiclabs/hardhat-ethers");
require("dotenv").config();

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

// defaultNetwork = "polygon";
/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
  solidity: "0.8.14",
  settings: {
    optimizer: {
      enabled: true,
      runs: 1000
    }
  },

  // networks: {
  //   polygon: {
  //     url: `${process.env.POLYGON_URL}`,
  //     accounts: [`0x${process.env.PRIVATE_KEY}`],
  //   },
  //   kovan: {
  //     url: `${process.env.KOVAN_URL}`,
  //     accounts: [`0x${process.env.PRIVATE_KEY}`],
  //   }
  // },
  // namedAccounts: {
  //   deployer: 0
  // }
};
