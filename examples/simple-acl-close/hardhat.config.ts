import { HardhatUserConfig } from "hardhat/config";
import { config as dotenvConfig } from "dotenv";
import "hardhat-prettier";

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
  paths: {
      sources: "./src"
  }
};

export default config;
