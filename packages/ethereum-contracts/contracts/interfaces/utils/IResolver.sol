// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

/**
 * @title Abstraction for an address resolver contract
 * @author Superfluid
 */
interface IResolver {

    /**
     * @dev NameSet event (DEPRECATED BY Set)
     * @param name Name set in resolver
     * @param target Target address
     *
     * @custom:deprecated Use Set instead
     */
    event NameSet(string indexed name, address target);

    event Set(string indexed name, address target);

    /**
     * @dev Set resolver address name
     */
    function set(string calldata name, address target) external;

    /**
     * @dev Get address by name
     */
    function get(string calldata name) external view returns (address);

}
