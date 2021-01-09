// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

/**
 * @dev Abstraction for a address resolver contract
 *
 * @author Superfluid
 */
interface IResolver {

    /**
     * @dev Get address by name.
     */
    function get(string calldata name) external view returns (address);

}
