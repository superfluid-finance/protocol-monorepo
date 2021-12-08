import { HardhatUserConfig } from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-waffle";

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
            url: "https://polygon-rpc.com/",
            chainId: 137,
        },
    },
    mocha: {
        timeout: 250000,
    },
};

export default config;
