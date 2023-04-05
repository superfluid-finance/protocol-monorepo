require("dotenv").config();
require("@nomiclabs/hardhat-ethers");
require("@nomiclabs/hardhat-etherscan");
require("hardhat-deploy");
require("hardhat/config");

// You need to export an object to set up your config
// Go to https://hardhat.org/config/ to learn more

/**
 * @type import('hardhat/config').HardhatUserConfig
 */
module.exports = {
    solidity: {
        version: "0.8.19",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            }
        }
    },
    networks: {
        localhost: {
            url: "http://127.0.0.1:8545/",
            chainId: 31337,
        },
        goerli: {
            url: process.env.GOERLI_URL || "",
            accounts:
                process.env.GOERLI_PRIVATE_KEY !== undefined ? [process.env.GOERLI_PRIVATE_KEY] : [],
        },
        mumbai: {
            url: process.env.MUMBAI_URL || "",
            accounts:
                process.env.MUMBAI_PRIVATE_KEY !== undefined ? [process.env.MUMBAI_PRIVATE_KEY] : [],
        },
        polygon: {
            url: process.env.POLYGON_URL || "",
            accounts:
                process.env.POLYGON_PRIVATE_KEY !== undefined ? [process.env.POLYGON_PRIVATE_KEY] : [],
        },
        bsc: {
            url: process.env.BSC_URL || "",
            accounts:
                process.env.BSC_PRIVATE_KEY !== undefined ? [process.env.BSC_PRIVATE_KEY] : [],
        },
    },

    namedAccounts: {
        deployer: {
            default: 0,
        },
    },
    etherscan: {
        apiKey: process.env.ETHERSCAN_API_KEY,
    },
};
