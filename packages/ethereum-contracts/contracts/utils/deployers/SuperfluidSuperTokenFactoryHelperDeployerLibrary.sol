// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ISuperfluid, ISuperfluidToken } from "../../superfluid/Superfluid.sol";
import {
    ISuperTokenFactory,
    SuperTokenFactory,
    SuperTokenFactoryHelper
} from "../../superfluid/SuperTokenFactory.sol";
import { TestResolver } from "../TestResolver.sol";

/// @title SuperfluidSuperTokenFactoryHelperDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid Super Token Factory Helper contract.
/// @dev This library is used for testing purposes only, not deployments to test OR production networks.
library SuperfluidSuperTokenFactoryHelperDeployerLibrary {
    /// @dev deploys Super Token Factory Helper contract
    /// @return newly deployed Super Token Factory Helper contract
    function deploySuperTokenFactoryHelper()
        external
        returns (SuperTokenFactoryHelper)
    {
        return new SuperTokenFactoryHelper();
    }
}
