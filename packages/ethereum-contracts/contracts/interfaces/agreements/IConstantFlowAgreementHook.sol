// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { ConstantFlowAgreementV1 } from "../../agreements/ConstantFlowAgreementV1.sol";
import { ISuperfluidToken } from "../superfluid/ISuperfluidToken.sol";

/// @title IConstantFlowAgreementHook interface
/// @author Superfluid
/// @notice An interface for the functions needed by a CFA hook contract
/// @dev The contract that implements this interface MUST only allow the CFA contract to call it
interface IConstantFlowAgreementHook {
    /// @notice A hook which executes on stream creation if the hook contract is set in the CFA
    /// @dev This should be implemented with an onlyCFA modifier, so that only the CFA can call the function
    /// @param newFlowData new flow data, see ConstantFlowAgreementV1 to see properties on FlowParams struct
    /// @param token the streamed super token
    /// @return bool
    function onCreate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token
    ) external returns (bool);

    /// @notice A hook which executes on stream update if the hook contract is set in the CFA
    /// @dev This should be implemented with an onlyCFA modifier, so that only the CFA can call the function
    /// @param newFlowData new flow data, see ConstantFlowAgreementV1 to see properties on FlowParams struct
    /// @param token the streamed super token
    /// @param oldFlowRate previous flowrate
    /// @return bool
    function onUpdate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external returns (bool);

    /// @notice A hook which executes on stream deletion if the hook contract is set in the CFA
    /// @dev This should be implemented with an onlyCFA modifier, so that only the CFA can call the function
    /// @param newFlowData new flow data, see ConstantFlowAgreementV1 to see properties on FlowParams struct
    /// @param token the streamed super token
    /// @param oldFlowRate previous flowrate
    /// @return bool
    function onDelete(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external returns (bool);
}
