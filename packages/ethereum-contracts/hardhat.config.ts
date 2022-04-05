import {HardhatUserConfig} from "hardhat/config";
import "@nomiclabs/hardhat-web3";
import "@nomiclabs/hardhat-waffle";
import "@nomiclabs/hardhat-truffle5";
import "solidity-coverage";
import {config as dotenvConfig} from "dotenv";

try {
    dotenvConfig();
} catch (error) {
    console.error(
        "Loading .env file failed. Things will likely fail. You may want to copy .env.template and create a new one."
    );
}

const config: HardhatUserConfig = {
    solidity: {
        version: "0.8.13",
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
            url: process.env.MATIC_PROVIDER_URL || "",
            chainId: 137,
        },
        mumbai: {
            url: process.env.MUMBAI_PROVIDER_URL || "",
            chainId: 80001,
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
        opkovan: {
            url: process.env.OPKOVAN_PROVIDER_URL || "",
            chainId: 69,
        },
        arbrinkeby: {
            url: process.env.ARBRINKEBY_PROVIDER_URL || "",
            chainId: 69,
        },
        coverage: {
            url: "http://127.0.0.1:8555",
        },
    },
    mocha: {
        timeout: 250000,
    },
};

export default config;
