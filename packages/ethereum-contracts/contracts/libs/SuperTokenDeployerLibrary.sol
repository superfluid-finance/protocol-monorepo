// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { ISuperfluid } from "../interfaces/superfluid/ISuperfluid.sol";
import { SuperToken } from "../superfluid/SuperToken.sol";

/// @title SuperToken deployer library
/// @author Superfluid
/// @notice This is an external library used to deploy SuperToken logic contracts
library SuperTokenDeployerLibrary {

    /// @notice Deploy a SuperToken logic contract
    /// @param host the address of the host contract
    function deploySuperTokenLogic(
        ISuperfluid host
    ) external returns (address) {
        return address(new SuperToken(host));
    }
}