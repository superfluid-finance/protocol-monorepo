// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { Resolver } from "./Resolver.sol";

/// @title TestResolver contract
/// @author Superfluid
/// @notice A Resolver specifically used for testing
/// @dev Used by the SuperfluidFrameworkDeployer to grant admin privileges to its deployer
contract TestResolver is Resolver {
    constructor(address _additionalAdmin) {
        _setupRole(DEFAULT_ADMIN_ROLE, _additionalAdmin);
    }
}
