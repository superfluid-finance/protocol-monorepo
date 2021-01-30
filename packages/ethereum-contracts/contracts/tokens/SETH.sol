// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperToken,
    CustomSuperTokenProxyBase
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
    function upgradeByETHTo(address to) external virtual payable;
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
contract SETHProxy is SETHProxyBase {

    IWETH immutable private _weth;

    constructor(IWETH weth) {
        _weth = weth;
    }

    function _implementation() internal override view returns (address)
    {
        if (msg.data.length > 0) {
            return UUPSProxy._implementation();
        } else {
            // do not provide receive() fallback otherwise it can't
            // withdraw WETH
            return address(0);
        }
    }

    function upgradeByETHTo(address to) external override payable {
        _weth.deposit{ value: msg.value }();
        ISuperToken(address(this)).selfMint(to, msg.value, new bytes(0));
    }

    function downgradeToETH(uint wad) external override {
        ISuperToken(address(this)).selfBurn(msg.sender, wad, new bytes(0));
        _weth.withdraw(wad);
        msg.sender.transfer(wad);
    }
}
