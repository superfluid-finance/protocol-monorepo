import { HardhatUserConfig, subtask } from "hardhat/config";
import { config as dotenvConfig } from "dotenv";
import "@nomiclabs/hardhat-ethers";
import "hardhat-prettier";
import "@nomiclabs/hardhat-etherscan";
import { TASK_COMPILE_SOLIDITY_GET_SOURCE_PATHS } from "hardhat/builtin-tasks/task-names";

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
        goerli: {
            url: process.env.GOERLI_PROVIDER_URL || "",
            chainId: 5,
            accounts: [process.env.PRIVATE_KEY || ""],
        },
    },
    paths: {
        sources: "./src",
    },
    etherscan: {
        apiKey: process.env.ETHERSCAN_API_KEY || "",
    },
};

export default config;

