import { HardhatUserConfig } from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-waffle";
import { config as dotenvConfig } from "dotenv";
dotenvConfig();

/**
 * This Hardhat config is only used for testing the subgraph.
 * Note: For tests to work, 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266
 * must be the deployer of the contracts. (add to readme.md).
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
            url: "http://0.0.0.0:8545/",
            chainId: 1337,
        },
        matic: {
            url: "https://polygon-mainnet.g.alchemy.com/v2/" + process.env.ALCHEMY_KEY,
            chainId: 137,
        },
        mumbai: {
            url: process.env.MUMBAI_PROVIDER_URL,
            chainId: 80001
        },
        ropsten: {
            url: process.env.ROPSTEN_PROVIDER_URL,
            chainId: 3
        },
        rinkeby: {
            url: process.env.RINKEBY_PROVIDER_URL,
            chainId: 4
        },
        goerli: {
            url: process.env.GOERLI_PROVIDER_URL,
            chainId: 5
        },
        opkovan: {
            url: process.env.OPKOVAN_PROVIDER_URL,
            chainId: 69
        }
    },
    mocha: {
        timeout: 250000,
    },
};

export default config;
