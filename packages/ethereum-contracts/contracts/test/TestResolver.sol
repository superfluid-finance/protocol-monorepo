// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import { AccessControl } from "@openzeppelin/contracts/access/AccessControl.sol";
import { IResolver } from "../interfaces/misc/IResolver.sol";


contract TestResolver is IResolver, AccessControl {

    mapping(string => address) private _registry;

    constructor() {
        _setupRole(DEFAULT_ADMIN_ROLE, msg.sender);
    }

    function set(string calldata name, address target) external {
        require(hasRole(DEFAULT_ADMIN_ROLE, _msgSender()), "Caller is not an admin");
        _registry[name] = target;
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
