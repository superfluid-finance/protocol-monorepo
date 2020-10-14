// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

import { AccessControl } from "@openzeppelin/contracts/access/AccessControl.sol";
import { IResolver } from "../interfaces/misc/IResolver.sol";


contract TestResolver is IResolver, AccessControl {

    bytes32 public constant ADMIN_ROLE = keccak256("MY_ROLE");

    mapping(string => address) private _registry;

    constructor() {
        _setupRole(ADMIN_ROLE, msg.sender);
    }

    function grantAdmin(address account) public virtual {
        require(hasRole(ADMIN_ROLE, _msgSender()), "AccessControl: sender must be an admin to grant");

        grantRole(ADMIN_ROLE, account);
    }

    function set(string calldata name, address target) external {
        require(hasRole(ADMIN_ROLE, _msgSender()), "Caller is not an admin");
        _registry[name] = target;
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
