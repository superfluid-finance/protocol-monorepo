import { HardhatUserConfig } from "hardhat/types";
import "../../../src/index";

const config: HardhatUserConfig = {
    solidity: "0.8.13",
    defaultNetwork: "hardhat",
    paths: {
        tests: "./test",
    },
    networks: {
        hardhat: {
            blockGasLimit: 100000000,
        },
    },
};

export default config;
