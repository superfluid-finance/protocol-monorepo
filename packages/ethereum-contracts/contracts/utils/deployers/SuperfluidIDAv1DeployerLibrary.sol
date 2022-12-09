// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    InstantDistributionAgreementV1
} from "../../agreements/InstantDistributionAgreementV1.sol";
import { ISuperfluid } from "../../superfluid/Superfluid.sol";

/// @title SuperfluidIDAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys the Superfluid InstantDistributionAgreementV1 contract.
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidIDAv1DeployerLibrary {
    /// @notice deploys the Superfluid InstantDistributionAgreementV1 Contract
    /// @param _host Superfluid host address
    /// @return newly deployed InstantDistributionAgreementV1 contract
    function deployInstantDistributionAgreementV1(
        ISuperfluid _host
    ) external returns (InstantDistributionAgreementV1) {
        return new InstantDistributionAgreementV1(_host);
    }
}
