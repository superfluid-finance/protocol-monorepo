import { HardhatUserConfig } from "hardhat/config";
import "@typechain/hardhat";
import "@nomiclabs/hardhat-ethers";
import "@nomiclabs/hardhat-waffle";
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
        version: "0.7.6",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            },
        },
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
    },
    mocha: {
        timeout: 250000,
    },
};

export default config;
