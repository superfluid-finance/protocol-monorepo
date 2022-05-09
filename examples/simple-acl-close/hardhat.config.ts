import { HardhatUserConfig, subtask, task, types } from "hardhat/config";
import { config as dotenvConfig } from "dotenv";
import "hardhat-prettier";
import "@nomiclabs/hardhat-etherscan";
import { TASK_COMPILE_SOLIDITY_GET_SOURCE_PATHS } from "hardhat/builtin-tasks/task-names";
import "@typechain/hardhat";
import "@nomiclabs/hardhat-web3";
import { verifyContract } from "./scripts/verify";
import "@nomiclabs/hardhat-ethers";
import "solidity-coverage";
import "hardhat-gas-reporter";

try {
    dotenvConfig();
} catch (error) {
    console.error(
        "Loading .env file failed. Things will likely fail. You may want to copy .env.template and create a new one.",
    );
}

// hardhat mixin magic: https://github.com/NomicFoundation/hardhat/issues/2306#issuecomment-1039452928
// filter out foundry test codes
subtask(TASK_COMPILE_SOLIDITY_GET_SOURCE_PATHS).setAction(
    async (_, __, runSuper) => {
        const paths = await runSuper();
        return paths.filter((p: string) => !p.endsWith(".t.sol"));
    },
);

/**
 * Verify Contract task
 * Run: `npx verifyContract --address <ADDRESS> --args '["0x..", 123, "0x..."]'`
 */
task("verifyContract", "Verify a contract")
    .addParam("address", "the contract address", "0x...", types.string)
    .addParam("args", "constructor arguments", {}, types.json)
    .setAction(async function (taskArgs, hre) {
        await verifyContract(hre, taskArgs.address, taskArgs.args);
    });

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
            chainId: 31337,
        },
        goerli: {
            url: process.env.PROVIDER_URL || "",
            chainId: 5,
            accounts: process.env.PRIVATE_KEY ? [process.env.PRIVATE_KEY] : [],
        },
    },
    paths: {
        sources: "./src",
    },
    etherscan: {
        apiKey: process.env.ETHERSCAN_API_KEY || "",
    },
    gasReporter: {
        enabled: (process.env.REPORT_GAS) ? true : false
    }
};

export default config;

