// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

import { IResolver } from "../interfaces/IResolver.sol";

/**
 * @title Gelato Ops Mock contract
 * @author Superfluid
 * @notice This contract is the executor which checks the resolver if it can close a stream and attempts to do so with
 *         the returned payload from checker().
 * @dev We base the method of calling off: https://etherscan.io/address/0x3c8859cee19d369ceac29ef6306106a92dd1bcab#code
 */
contract OpsMock {
    IResolver internal resolver;
    address internal execAddress;

    error CannotExecute();
    error FailedExecution();

    constructor(address _resolver, address _execAddress) {
        resolver = IResolver(_resolver);
        execAddress = _execAddress;
    }

    function exec() external {
        (bool canExec, bytes memory execPayload) = resolver.checker();
        if (!canExec) {
            revert CannotExecute();
        }

        (bool success, ) = execAddress.call(execPayload);

        if (!success) {
            revert FailedExecution();
        }
    }
}
