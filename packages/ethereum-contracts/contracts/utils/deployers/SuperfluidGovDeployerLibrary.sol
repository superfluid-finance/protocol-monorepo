// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ISuperfluid, ISuperfluidToken } from "../../superfluid/Superfluid.sol";
import { TestGovernance } from "../TestGovernance.sol";

/// @title SuperfluidGovDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid TestGovernance contract with additional functions
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidGovDeployerLibrary {
    /// @notice deploys the Superfluid TestGovernance Contract
    /// @return newly deployed TestGovernance contract
    function deployTestGovernance() external returns (TestGovernance) {
        return new TestGovernance();
    }

    /// @notice transfers ownership of _gov to _newOwner
    /// @dev _gov must be deployed from this contract
    /// @param _gov address of the TestGovernance contract
    /// @param _newOwner the new owner of the governance contract
    function transferOwnership(
        TestGovernance _gov,
        address _newOwner
    ) external {
        _gov.transferOwnership(_newOwner);
    }
}
