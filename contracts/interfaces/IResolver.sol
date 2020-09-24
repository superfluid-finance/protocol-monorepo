// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

/**
 * @dev Abstraction for a address resolver contract
 *
 * @author Superfluid
 */
interface IResolver {

    /**
     * @dev Set name to the target.
     */
    function set(string calldata name, address target) external;

    /**
     * @dev Get address by name.
     */
    function get(string calldata name) external view returns (address);

}
