// SPDX-License-Identifier: MIT
pragma solidity >= 0.8.11;

// We export this too because it seems reasonable for a custom super token.
// solhint-disable-next-line no-unused-import
import { ISuperToken } from "../../interfaces/superfluid/ISuperToken.sol";

/**
 * @title Custom super token base contract
 * @author Superfluid
 * NOTE:
 * - Because of how solidity lays out its storage variables and how custom
 *   super tokens inherit the SuperToken standard implementation, it is
 *   required that the custom token proxy pads its implementation
 *   with reserved storage used by the Super Token implementation.
 * - You will need to append your own proxy implementation after the base
 *   - Refer to SETH.sol for an example how it is used.
 */
abstract contract CustomSuperTokenBase {
    // This (32) is the hard-coded number of storage slots used by the super token
    uint256[32] internal _storagePaddings;
}
