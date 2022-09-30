// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.0;

import { IConstantFlowAgreementHook } from "../interfaces/agreements/IConstantFlowAgreementHook.sol";
import { ISuperfluidToken } from "../interfaces/superfluid/ISuperfluidToken.sol";
import { ConstantFlowAgreementV1 } from "../agreements/ConstantFlowAgreementV1.sol";

/// @title BaseCFAHookMock abstract contract
/// @author Superfluid
/// @notice An abstract contract with functionality inherited by both hook mocks
/// @dev This should provide a good starting point of some of the properties the CFAHook contract should have
abstract contract BaseCFAHookMock is IConstantFlowAgreementHook {
    ConstantFlowAgreementV1 internal cfaV1;
    address internal immutable owner;

    // Custom Errors
    error NOT_CFA();
    error NOT_OWNER();

    modifier onlyCFA() {
        if (msg.sender != address(cfaV1)) revert NOT_CFA();
        _;
    }

    modifier onlyOwner() {
        if (msg.sender != owner) revert NOT_OWNER();
        _;
    }

    constructor() {
        owner = msg.sender;
    }

    /// @notice Sets the CFA contract which can call the hooks
    /// @dev Only the owner of the contract can set the CFA contract
    /// @param _cfaV1 address of cfav1 contract
    function setCFA(ConstantFlowAgreementV1 _cfaV1) external onlyOwner {
        cfaV1 = _cfaV1;
    }
}

/// @title GoodCFAHookMock contract
/// @author Superfluid
/// @dev A "good" mock contract which just emits an event in the hooks and returns true
contract GoodCFAHookMock is BaseCFAHookMock {
    event OnCreateEvent(
        ISuperfluidToken token,
        address sender,
        address receiver,
        address flowOperator,
        int96 flowRate,
        int96 oldFlowRate
    );
    event OnUpdateEvent(
        ISuperfluidToken token,
        address sender,
        address receiver,
        address flowOperator,
        int96 flowRate,
        int96 oldFlowRate
    );
    event OnDeleteEvent(
        ISuperfluidToken token,
        address sender,
        address receiver,
        address flowOperator,
        int96 flowRate,
        int96 oldFlowRate
    );

    function onCreate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external onlyCFA returns (bool) {
        emit OnCreateEvent(
            token,
            newFlowData.sender,
            newFlowData.receiver,
            newFlowData.flowOperator,
            newFlowData.flowRate,
            oldFlowRate
        );

        return true;
    }

    function onUpdate(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external onlyCFA returns (bool) {
        emit OnUpdateEvent(
            token,
            newFlowData.sender,
            newFlowData.receiver,
            newFlowData.flowOperator,
            newFlowData.flowRate,
            oldFlowRate
        );

        return true;
    }

    function onDelete(
        ConstantFlowAgreementV1.FlowParams memory newFlowData,
        ISuperfluidToken token,
        int96 oldFlowRate
    ) external onlyCFA returns (bool) {
        emit OnDeleteEvent(
            token,
            newFlowData.sender,
            newFlowData.receiver,
            newFlowData.flowOperator,
            newFlowData.flowRate,
            oldFlowRate
        );

        return true;
    }
}

/// @title BadCFAHookMock contract
/// @author Superfluid
/// @dev A "bad" mock contract which just reverts
contract BadCFAHookMock is BaseCFAHookMock {
    error BAD_HOOK();

    function onCreate(
        ConstantFlowAgreementV1.FlowParams memory, // newFlowData,
        ISuperfluidToken, // token,
        int96 // oldFlowRate
    ) external view onlyCFA returns (bool) {
        revert BAD_HOOK();
    }

    function onUpdate(
        ConstantFlowAgreementV1.FlowParams memory, // newFlowData,
        ISuperfluidToken, // token,
        int96 // oldFlowRate
    ) external view onlyCFA returns (bool) {
        revert BAD_HOOK();
    }

    function onDelete(
        ConstantFlowAgreementV1.FlowParams memory, // newFlowData,
        ISuperfluidToken, // token,
        int96 // oldFlowRate
    ) external view onlyCFA returns (bool) {
        revert BAD_HOOK();
    }
}
