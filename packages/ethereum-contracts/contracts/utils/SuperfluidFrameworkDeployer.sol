// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { IResolver } from "../interfaces/utils/IResolver.sol";
import { CFAv1Library } from "../apps/CFAv1Library.sol";
import { IDAv1Library } from "../apps/IDAv1Library.sol";
import { Superfluid } from "../superfluid/Superfluid.sol";
import { TestResolver } from "./TestResolver.sol";
import { SuperfluidLoader } from "./SuperfluidLoader.sol";
import {
    SuperfluidFrameworkDeploymentSteps
} from "./SuperfluidFrameworkDeploymentSteps.sol";

/// @title Superfluid Framework Deployer
/// @notice This is NOT for deploying public nets, but rather only for tesing envs
contract SuperfluidFrameworkDeployer is SuperfluidFrameworkDeploymentSteps {
    constructor() {
        _executeAllSteps();
    }
}
