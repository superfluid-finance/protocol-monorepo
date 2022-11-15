// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { ISuperToken } from "../superfluid/ISuperToken.sol";

/**
 * @title Native SuperToken custom interface
 * @author Superfluid
 */
interface INativeSuperTokenCustom {
    function initialize(string calldata name, string calldata symbol, uint256 initialSupply) external;
}

/**
 * @title Native SuperToken interface
 * @author Superfluid
 */
interface INativeSuperToken is INativeSuperTokenCustom, ISuperToken {
    function initialize(string calldata name, string calldata symbol, uint256 initialSupply) external override;
}
