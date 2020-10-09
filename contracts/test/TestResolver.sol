// SPDX-License-Identifier: MIT
pragma solidity 0.7.3;

import { IResolver } from "../interfaces/misc/IResolver.sol";


contract TestResolver is IResolver {

    mapping(string => address) private _registry;

    function set(string calldata name, address target) external {
        _registry[name] = target;
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
