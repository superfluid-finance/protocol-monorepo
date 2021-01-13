// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.7.0;

import { ISuperToken } from "./ISuperToken.sol";


abstract contract CustomSuperTokenProxyBase {
    // This is the hard coded number of storage slots used by the super token
    uint256[32] internal _storagePaddings;
}
