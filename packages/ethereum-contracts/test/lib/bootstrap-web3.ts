import {extendEnvironment} from "hardhat/config";

/** Full web3 v1 on global for @decentral.ee/web3-helpers web3tx (replaces test-helpers configure). */
extendEnvironment((env) => {
    const Web3 = require("web3");
    const fullWeb3 = new Web3(
        env.network.provider as Parameters<
            InstanceType<typeof Web3>["setProvider"]
        >[0]
    );
    (global as typeof globalThis & {web3?: InstanceType<typeof Web3>}).web3 =
        fullWeb3;
});
