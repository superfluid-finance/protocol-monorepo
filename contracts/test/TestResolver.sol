// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.1;

import { AccessControl } from "@openzeppelin/contracts/access/AccessControl.sol";
import { IResolver } from "../interfaces/misc/IResolver.sol";


contract TestResolver is IResolver, AccessControl {

    bytes32 public constant RESOLVER_ADMIN_ROLE = keccak256("RESOLVER_ADMIN_ROLE");

    mapping(string => address) private _registry;

    constructor() {
        _setupRole(RESOLVER_ADMIN_ROLE, msg.sender);
    }

    function isAdmin(address account) external view returns (bool) {
        return hasRole(RESOLVER_ADMIN_ROLE, account);
    }

    function grantAdmin(address account) external {
        require(hasRole(RESOLVER_ADMIN_ROLE, _msgSender()), "AccessControl: sender must be an admin to grant");

        _setupRole(RESOLVER_ADMIN_ROLE, account);
    }

    function set(string calldata name, address target) external {
        require(hasRole(RESOLVER_ADMIN_ROLE, _msgSender()), "Caller is not an admin");
        _registry[name] = target;
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
