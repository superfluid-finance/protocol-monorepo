require("@typechain/hardhat");
require("@nomiclabs/hardhat-ethers");
require("@nomicfoundation/hardhat-chai-matchers");
require("@nomiclabs/hardhat-web3");
require("dotenv").config();

/**
 * This Hardhat config is only used for testing the SDK-Core.
 * Note: For tests to work, 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266
 * must be the deployer of the contracts.
 */
module.exports = {
    solidity: {
        version: "0.8.15",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            },
        },
    },
    paths: {
        artifacts: "../ethereum-contracts/artifacts",
    },
    networks: {
        localhost: {
            // url may not be necessary for localhost/hardhat node chainId
            url: "http://0.0.0.0:8545/",
            chainId: 31337,
        },
        matic: {
            url: process.env.MATIC_PROVIDER_URL || "",
            chainId: 137,
        },
        bsc: {
            url: process.env.BSC_PROVIDER_URL || "",
            accounts: process.env.TEST_PRIVATE_KEY
                ? [process.env.TEST_PRIVATE_KEY || ""]
                : [],
            chainId: 56,
        },
    },
    mocha: {
        timeout: 250000,
    },
};
