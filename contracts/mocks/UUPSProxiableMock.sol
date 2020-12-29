// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import { UUPSProxiable } from "../upgradability/UUPSProxiable.sol";


contract UUPSProxiableMock is UUPSProxiable {

    bytes32 private immutable _uuid;
    uint256 public immutable waterMark;

    constructor(bytes32 uuid, uint256 w) {
        _uuid = uuid;
        waterMark = w;
    }

    function proxiableUUID() public view override returns (bytes32)
    {
        return _uuid;
    }

    function updateCode(address newAddress) external override
    {
        _updateCodeAddress(newAddress);
    }
}
