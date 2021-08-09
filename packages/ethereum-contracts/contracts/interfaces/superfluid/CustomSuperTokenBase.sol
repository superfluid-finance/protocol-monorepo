// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.7.0;

import {
    ISuperToken
} from "../../interfaces/superfluid/ISuperToken.sol";


/**
 * @dev Custom super token base contract
 *
 * NOTE:
 * - Because of how solidity is layouting its storages variables and custom
 *   super token inherits the Super Token standard implementation, so it is
 *   required that the custom token proxy would need to pad its implementation
 *   with reserved storages used by the Super Token implementation.
 * - You will need to append your own proxy implementation after the base
 *   - Refer to SETH.sol for an example how it is used.
 */
abstract contract CustomSuperTokenBase {
    // This is the hard-coded number of storage slots used by the super token
    uint256[32] internal _storagePaddings;
}
