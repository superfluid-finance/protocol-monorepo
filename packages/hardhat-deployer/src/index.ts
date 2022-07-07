import { extendEnvironment } from "hardhat/config";
import { lazyObject } from "hardhat/plugins";

import { SuperfluidFrameworkDeployer } from "./SuperfluidFrameworkDeployer";

import "./type-extensions";

extendEnvironment((hre) => {
    // We add a field to the Hardhat Runtime Environment here.
    // We use lazyObject to avoid initializing things until they are actually
    // needed.
    hre.superfluidFrameworkDeployer = lazyObject(
        () => new SuperfluidFrameworkDeployer()
    );
});
