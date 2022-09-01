// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.16;

import { AccessControlEnumerable } from "@openzeppelin/contracts/access/AccessControlEnumerable.sol";
import { IResolver } from "../interfaces/utils/IResolver.sol";


/**
 * @title Resolver contract
 * @author Superfluid
 * @dev A simple implementation of IResolver using OZ AccessControl
 *
 * NOTE:
 * Relevant events for indexing:
 * - OZ Access Control events `RoleGranted`/`RoleRevoked`: admin add/remove
 * - IResolver event `Set`: resolver name updates
 */
contract Resolver is IResolver, AccessControlEnumerable {

    mapping(string => address) private _registry;

    constructor() {
        _setupRole(DEFAULT_ADMIN_ROLE, msg.sender);
    }

    function set(string calldata name, address target) external override {
        require(hasRole(DEFAULT_ADMIN_ROLE, _msgSender()), "Caller is not an admin");
        _registry[name] = target;
        emit Set(name, target);
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
