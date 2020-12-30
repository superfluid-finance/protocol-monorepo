// SPDX-License-Identifier: MIT
pragma solidity 0.7.6;

import {
    ISuperToken
} from "../superfluid/SuperToken.sol";

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
interface SETHFunctions {
    function upgradeByETH() external payable;
    function downgradeToETH(uint wad) external;
}

/**
 * @dev Super ETH (SETH) custom super totken interface
 *
 * @author Superfluid
 */
// solhint-disable-next-line no-empty-blocks
interface SETH is SETHFunctions, ISuperToken { }

/**
 * @dev Super ETH (SETH) custom super totken implementation
 *
 * @author Superfluid
 */
contract SETHProxy is SETHFunctions, UUPSProxy {

    IWETH immutable private _weth;

    constructor(IWETH weth) {
        _weth = weth;
    }

    function upgradeByETH() external override payable {
        ISuperToken(address(this)).mint(msg.sender, msg.value, new bytes(0));
    }

    function downgradeToETH(uint wad) external override {
        ISuperToken(address(this)).burn(msg.sender, wad, new bytes(0));
        msg.sender.transfer(wad);
    }
}
