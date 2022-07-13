import { HardhatUserConfig } from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-ethers";
import "@nomiclabs/hardhat-waffle";
import { config as dotenvConfig } from "dotenv";
dotenvConfig();

/**
 * This Hardhat config is only used for testing the subgraph.
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
        ropsten: {
            url: process.env.ROPSTEN_PROVIDER_URL || "",
            chainId: 3,
        },
        rinkeby: {
            url: process.env.RINKEBY_PROVIDER_URL || "",
            chainId: 4,
        },
        goerli: {
            url: process.env.GOERLI_PROVIDER_URL || "",
            chainId: 5,
        },
        kovan: {
            url: process.env.KOVAN_PROVIDER_URL || "",
            chainId: 42,
        },
        opkovan: {
            url: process.env.OPKOVAN_PROVIDER_URL || "",
            chainId: 69,
        },
        avafuji: {
            url: process.env.AVAFUJI_PROVIDER_URL || "",
            chainId: 43113,
        },
        mumbai: {
            url: process.env.MUMBAI_PROVIDER_URL || "",
            chainId: 80001,
        },
        arbrinkeby: {
            url: process.env.ARBRINKEBY_PROVIDER_URL || "",
            chainId: 421611,
        },
    },
    mocha: {
        timeout: 500000,
    },
};

export default config;
