// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ISuperfluid, ISuperfluidToken } from "../../superfluid/Superfluid.sol";
import {
    ISuperToken,
    ISuperTokenFactory,
    SuperTokenFactory
} from "../../superfluid/SuperTokenFactory.sol";
import { TestResolver } from "../TestResolver.sol";

/// @title SuperfluidPeripheryDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid periphery contracts (Super Token Factory and Test Resolver)
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidPeripheryDeployerLibrary {
    /// @dev deploys Super Token Factory contract
    /// @param _host address of the Superfluid contract
    /// @param _superTokenLogic address of the Super Token logic contract
    /// @return newly deployed SuperTokenFactory contract
    function deploySuperTokenFactory(
        ISuperfluid _host,
        ISuperToken _superTokenLogic
    ) external returns (SuperTokenFactory) {
        return new SuperTokenFactory(_host, _superTokenLogic);
    }

    /// @dev deploys Test Resolver contract
    /// @param _additionalAdmin address of the additional administrator of the Test Resolver contract
    /// @return newly deployed Test Resolver contract
    function deployTestResolver(
        address _additionalAdmin
    ) external returns (TestResolver) {
        return new TestResolver(_additionalAdmin);
    }
}
