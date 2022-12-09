import { HardhatUserConfig } from "hardhat/config";
import "@typechain/hardhat";
import "@nomiclabs/hardhat-ethers";
import "@nomicfoundation/hardhat-chai-matchers";
import "@nomiclabs/hardhat-web3";
import { config as dotenvConfig } from "dotenv";
dotenvConfig();

/**
 * This Hardhat config is only used for testing the SDK-Core.
 * Note: For tests to work, 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266
 * must be the deployer of the contracts.
 */
const config: HardhatUserConfig = {
    solidity: {
        version: "0.8.15",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            },
        },
    },
    networks: {
        hardhat: {
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

export default config;
