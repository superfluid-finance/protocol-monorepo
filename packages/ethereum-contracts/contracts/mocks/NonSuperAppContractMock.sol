// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import {ISuperApp, ISuperfluid} from "../superfluid/Superfluid.sol";

/// @title Non SuperApp Contract Mock
/// @author Superfluid
/// @notice This contract is used to test call app action against a non super app contract
/// @dev It is important to note that callAppAction and callAppActionWithContext is allowed to be called
/// by any contract. But, the `function` you want to invokeCallAppAction on must satisfy two requirements:
/// 1. The `function` you want to call via callAppAction must take context bytes as the last parameter.
/// 2. You must return the "correct" context bytes from the contract in the `function`, that is, untouched.
contract NonSuperAppContractMock {
    ISuperfluid public superfluid;
    constructor(ISuperfluid _superfluid) {
        superfluid = _superfluid;
    }

    event Log(uint256 amount);

    /// @notice An example of a function that does not satisfy the requirements for callAppAction
    /// @dev This does not satsify requirements 1 or 2
    /// @param _amount arbitrary amount
    function invalidCallAppActionFunction(uint256 _amount) external {
        emit Log(_amount);
    }

    /// @notice An example of a sneaky function that does not satisfy the requirements for callAppAction
    /// @dev This satisfies requirement 1 but not 2
    /// @param _amount arbitrary amount
    /// @param _ctx arbitrary context bytes
    /// @return newCtx the context bytes
    function sneakyCallAppActionFunction(
        uint256 _amount,
        bytes calldata //_ctx
    ) external returns (bytes memory newCtx) {
        emit Log(_amount);
    }


    /// @notice An example of a function that satisfies the requirements for callAppAction
    /// @dev This satisfies requirements 1 and 2
    /// @param _amount arbitrary amount
    /// @param _ctx correct context bytes
    /// @return newCtx the context bytes
    function validCallAppActionFunction(
        uint256 _amount,
        bytes calldata _ctx
    ) external returns (bytes memory newCtx) {
        emit Log(_amount);
        newCtx = _ctx;
    }
}
