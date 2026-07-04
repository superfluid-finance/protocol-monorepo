import {extendEnvironment} from "hardhat/config";
import {lazyObject} from "hardhat/plugins";

import "hardhat/types";

declare module "hardhat/types/runtime" {
    interface HardhatRuntimeEnvironment {
        web3: import("./web3-shim").web3;
    }
}

extendEnvironment((env) => {
    env.web3 = lazyObject(() => {
        const {web3} = require("./web3-shim");
        return web3;
    });
});
