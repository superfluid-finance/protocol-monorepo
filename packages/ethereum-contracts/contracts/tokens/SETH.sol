// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    CustomSuperTokenProxyBase,
    ISuperToken
}
from "../interfaces/superfluid/CustomSuperTokenProxyBase.sol";

import { UUPSProxy } from "../upgradability/UUPSProxy.sol";


/**
 * @dev Wrapped ETH interface
 */
interface IWETH {
    function deposit() external payable;
    function withdraw(uint wad) external;
}

/**
 * @dev Super ETH (SETH) specific functions
 *
 * @author Superfluid
 */
abstract contract SETHProxyBase is CustomSuperTokenProxyBase {
    function upgradeByETH() external virtual payable;
    function downgradeToETH(uint wad) external virtual;
}

/**
 * @dev Super ETH (SETH) custom super totken interface
 *
 * @author Superfluid
 */
// solhint-disable-next-line no-empty-blocks
abstract contract SETH is SETHProxyBase, ISuperToken { }

/**
 * @dev Super ETH (SETH) custom super totken implementation
 *
 * @author Superfluid
 */
contract SETHProxy is SETHProxyBase, UUPSProxy {

    IWETH immutable private _weth;

    constructor(IWETH weth) {
        _weth = weth;
    }

    function upgradeByETH() external override payable {
        ISuperToken(address(this)).selfMint(msg.sender, msg.value, new bytes(0));
    }

    function downgradeToETH(uint wad) external override {
        ISuperToken(address(this)).selfBurn(msg.sender, wad, new bytes(0));
        msg.sender.transfer(wad);
    }
}
