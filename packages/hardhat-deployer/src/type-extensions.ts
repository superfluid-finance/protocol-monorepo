import "@nomiclabs/hardhat-ethers";
import "hardhat/types/runtime";

import { SuperfluidFrameworkDeployer } from "./SuperfluidFrameworkDeployer";

declare module "hardhat/types/runtime" {
    // This new field will be available in tasks' actions, scripts, and tests.
    export interface HardhatRuntimeEnvironment {
        superfluidFrameworkDeployer: SuperfluidFrameworkDeployer;
    }
}
