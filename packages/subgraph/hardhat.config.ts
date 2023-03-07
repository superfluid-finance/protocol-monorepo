import { HardhatUserConfig } from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-ethers";
import "@nomicfoundation/hardhat-chai-matchers";
import { config as dotenvConfig } from "dotenv";
dotenvConfig();

/**
 * This Hardhat config is only used for testing the subgraph.
 * Note: For tests to work, 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266
 * must be the deployer of the contracts.
 *
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
        "optimism-mainnet": {
            url: process.env.OPTIMISM_PROVIDER_URL || "",
            chainId: 10,
        },
        gnosis: {
            url: process.env.GNOSIS_PROVIDER_URL || "",
            chainId: 100,
        },
        matic: {
            url: process.env.MATIC_PROVIDER_URL || "",
            chainId: 137,
        },
        "arbitrum-one": {
            url: process.env.ARBITRUM_ONE_PROVIDER_URL || "",
            chainId: 42161,
        },
        "avalanche-c": {
            url: process.env.AVALANCHE_C_PROVIDER_URL || "",
            chainId: 43114,
        },
        "bsc-mainnet": {
            url: process.env.BSC_MAINNET_PROVIDER_URL || "",
            chainId: 56,
        },
        "celo-mainnet": {
            url: process.env.CELO_MAINNET_PROVIDER_URL || "",
            chainId: 42220,
        },
        goerli: {
            url: process.env.GOERLI_PROVIDER_URL || "",
            chainId: 5,
        },
        avafuji: {
            url: process.env.AVAFUJI_PROVIDER_URL || "",
            chainId: 43113,
        },
        mumbai: {
            url: process.env.MUMBAI_PROVIDER_URL || "",
            chainId: 80001,
        },
    },
    mocha: {
        timeout: 500000,
    },
};

export default config;
