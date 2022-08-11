import {HardhatUserConfig, subtask} from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-waffle";
import "@nomiclabs/hardhat-truffle5";
import {TASK_COMPILE_SOLIDITY_GET_SOURCE_PATHS} from "hardhat/builtin-tasks/task-names";
import "solidity-coverage";
import {config as dotenvConfig} from "dotenv";
import {NetworkUserConfig} from "hardhat/types";
import "solidity-docgen";
import { resolve, relative } from "path";

try {
    dotenvConfig();
} catch (error) {
    console.error(
        "Loading .env file failed. Things will likely fail. You may want to copy .env.template and create a new one."
    );
}

// hardhat mixin magic: https://github.com/NomicFoundation/hardhat/issues/2306#issuecomment-1039452928
// filter out foundry test codes
subtask(TASK_COMPILE_SOLIDITY_GET_SOURCE_PATHS).setAction(
    async (_, __, runSuper) => {
        const paths = await runSuper();
        return paths.filter((p: string) => !p.includes("test/foundry"));
    }
);

const chainIds = {
    "eth-mainnet": 1,
    "eth-goerli": 5,

    "xdai-mainnet": 100,

    "optimism-mainnet": 10,
    // TODO: add optimism-goerli

    "arbitrum-one": 42161,
    // TODO: add arbitrum-goerli

    "polygon-mainnet": 137,
    "polygon-mumbai": 80001,

    "avalanche-c": 43114,
    "avalanche-fuji": 43113,

    "bsc-mainnet": 56,

    localhost: 1337,
    hardhat: 31337,
};

function createNetworkConfig(
    network: keyof typeof chainIds
): NetworkUserConfig {
    return {
        accounts:
            process.env.PRIVATE_KEY !== undefined
                ? [process.env.PRIVATE_KEY]
                : [],
        chainId: chainIds[network],
    };
}

const config: HardhatUserConfig = {
    solidity: {
        version: "0.8.14",
        settings: {
            optimizer: {
                enabled: true,
                runs: 200,
            },
        },
    },
    networks: {
        localhost: {
            ...createNetworkConfig("localhost"),
            url: "http://0.0.0.0:8545/",
        },
        "bsc-mainnet": {
            ...createNetworkConfig("bsc-mainnet"),
            url: process.env.BSC_PROVIDER_URL || "",
        },
        "eth-goerli": {
            ...createNetworkConfig("eth-goerli"),
            url: process.env.GOERLI_PROVIDER_URL || "",
        },
        "xdai-mainnet": {
            ...createNetworkConfig("xdai-mainnet"),
            url: process.env.XDAI_MAINNET_PROVIDER_URL || "",
        },
        "optimism-mainnet": {
            ...createNetworkConfig("optimism-mainnet"),
            url: process.env.OPKOVAN_PROVIDER_URL || "",
        },
        // TODO: add optimism-goerli
        "arbitrum-one": {
            ...createNetworkConfig("arbitrum-one"),
            url: process.env.ARBONE_PROVIDER_URL || "",
        },
        // TODO: add arbitrum-goerli
        "polygon-mainnet": {
            ...createNetworkConfig("polygon-mainnet"),
            url: process.env.MATIC_PROVIDER_URL || "",
        },
        "polygon-mumbai": {
            ...createNetworkConfig("polygon-mumbai"),
            url: process.env.MUMBAI_PROVIDER_URL || "",
        },
        "avalanche-c": {
            ...createNetworkConfig("avalanche-c"),
            url: process.env.AVALANCHE_PROVIDER_URL || "",
        },
        "avalanche-fuji": {
            ...createNetworkConfig("avalanche-fuji"),
            url: process.env.AVALANCHE_PROVIDER_URL || "",
        },
        coverage: {
            url: "http://127.0.0.1:8555",
        },
    },
    mocha: {
        timeout: 250000,
    },
    docgen: {
        outputDir: "docs/api",
        templates: "./docs/docgen-templates",
        pages: (item: any, file: any) => file.absolutePath.startsWith('contracts/interfaces/')
            ? relative('contracts', file.absolutePath).replace('.sol', '.md')
            : undefined,
    },
};

export default config;
