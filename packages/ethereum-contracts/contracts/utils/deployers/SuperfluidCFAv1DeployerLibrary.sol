// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import {
    ConstantFlowAgreementV1
} from "../../agreements/ConstantFlowAgreementV1.sol";
import {
    IConstantFlowAgreementHook
} from "../../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { ISuperfluid, Superfluid } from "../../superfluid/Superfluid.sol";
import { TestGovernance } from "../TestGovernance.sol";

/// @title SuperfluidCFAv1DeployerLibrary
/// @author Superfluid
/// @notice An external library that deploys Superfluid ConstantFlowAgreementV1 contract
/// @dev This library is used for testing purposes only, not deployments to test OR production networks
library SuperfluidCFAv1DeployerLibrary {
    /// @notice deploys ConstantFlowAgreementV1 contract
    /// @param _host address of the Superfluid contract
    /// @param _cfaHook address of the IConstantFlowAgreementHook contract
    /// @return newly deployed ConstantFlowAgreementV1 contract
    function deployConstantFlowAgreementV1(
        ISuperfluid _host,
        IConstantFlowAgreementHook _cfaHook
    ) external returns (ConstantFlowAgreementV1) {
        return new ConstantFlowAgreementV1(_host, _cfaHook);
    }
}
