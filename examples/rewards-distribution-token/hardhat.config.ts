import * as dotenv from "dotenv";

import { HardhatUserConfig, task } from "hardhat/config";
import "@nomiclabs/hardhat-etherscan";
import "@nomiclabs/hardhat-ethers";
import "@nomiclabs/hardhat-waffle";
import "@typechain/hardhat";
import "solidity-coverage";

dotenv.config();

// This is a sample Hardhat task. To learn how to create your own go to
// https://hardhat.org/guides/create-task.html
task("accounts", "Prints the list of accounts", async (taskArgs, hre) => {
  const accounts = await hre.ethers.getSigners();

  for (const account of accounts) {
    console.log(account.address);
  }
});

const { API_URL, PRIVATE_KEY } = process.env;
module.exports = {
  solidity: "0.8.13",
  defaultNetwork: "ropsten",
  // networks: {
  //    hardhat: {},
  //    ropsten: {
  //       url: API_URL,
  //       accounts: [`0x${PRIVATE_KEY}`]
  //    }
  // },
}

// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

const config: HardhatUserConfig = {
  solidity: "0.8.4",
  // networks: {
  //   ropsten: {
  //     url: process.env.ROPSTEN_URL || "",
  //     accounts:
  //       process.env.PRIVATE_KEY !== undefined ? [process.env.PRIVATE_KEY] : [],
  //   },
  // },
  
  // etherscan: {
  //   apiKey: process.env.ETHERSCAN_API_KEY,
  // },
};

export default config;
