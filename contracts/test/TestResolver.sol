// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.1;

import { IResolver } from "../interfaces/misc/IResolver.sol";


contract TestResolver is IResolver {

    mapping(string => address) private _registry;

    function set(string calldata name, address target) external override {
        _registry[name] = target;
    }

    function get(string calldata name) external view override returns (address) {
        return _registry[name];
    }

}
