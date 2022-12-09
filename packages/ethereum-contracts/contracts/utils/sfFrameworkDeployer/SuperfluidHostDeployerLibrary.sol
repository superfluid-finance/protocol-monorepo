// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    ISuperfluid,
    ISuperfluidToken,
    Superfluid
} from "../../superfluid/Superfluid.sol";
import { TestGovernance } from "../TestGovernance.sol";

/// @title SuperfluidHostDeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid Host contract with additional functions.
/// @dev This library is used for testing purposes only, not deployments to test OR production networks.
library SuperfluidHostDeployerLibrary {
    /// @notice Deploys the Superfluid Host Contract
    /// @param _nonUpgradable whether the hsot contract is upgradeable or not
    /// @param _appWhiteListingEnabled whether app white listing is enabled
    /// @return Superfluid newly deployed Superfluid Host contract
    function deploySuperfluidHost(
        bool _nonUpgradable,
        bool _appWhiteListingEnabled
    ) external returns (Superfluid) {
        return new Superfluid(_nonUpgradable, _appWhiteListingEnabled);
    }
}
