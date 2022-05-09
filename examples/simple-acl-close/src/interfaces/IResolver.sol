// SPDX-License-Identifier: MIT
pragma solidity 0.8.13;

/**
 * @title IResolver Interface - Gelato resolver
 */
interface IResolver {
    function checker()
        external
        view
        returns (bool canExec, bytes memory execPayload);
}
