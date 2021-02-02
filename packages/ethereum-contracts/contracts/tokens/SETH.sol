// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;

import {
    ISuperToken,
    CustomSuperTokenProxyBase
}
from "../interfaces/superfluid/CustomSuperTokenProxyBase.sol";
import { IERC20 } from "@openzeppelin/contracts/token/ERC20/IERC20.sol";
import { UUPSProxy } from "../upgradability/UUPSProxy.sol";


/**
 * @dev Wrapped ETH interface
 */
interface IWETH is IERC20 {
    function deposit() external payable;
    function withdraw(uint wad) external;
}


/**
 * @dev Super ETH (SETH) custom token functions
 *
 * @author Superfluid
 */
interface ISETHCustom {
    function upgradeByETH() external payable;
    function upgradeByETHTo(address to) external payable;
    function upgradeByWETH(IWETH weth, uint wad) external;
    function downgradeToETH(uint wad) external;
    function downgradeToWETH(IWETH weth, uint wad) external;
}

/**
 * @dev Super ETH (SETH) full interface
 *
 * @author Superfluid
 */
// solhint-disable-next-line no-empty-blocks
interface ISETH is ISETHCustom, ISuperToken { }


/**
 * @dev Super ETH (SETH) custom super totken implementation
 *
 * @author Superfluid
 */
contract SETHProxy is ISETHCustom, CustomSuperTokenProxyBase {

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

    function upgradeByETH() external override payable {
        ISuperToken(address(this)).selfMint(msg.sender, msg.value, new bytes(0));
    }

    function upgradeByETHTo(address to) external override payable {
        ISuperToken(address(this)).selfMint(to, msg.value, new bytes(0));
    }

    function upgradeByWETH(IWETH weth, uint wad) external override {
        weth.transferFrom(msg.sender, address(this), wad);
        // this will trigger receive() which is overriden to a no-op
        weth.withdraw(wad);
        ISuperToken(address(this)).selfMint(msg.sender, wad, new bytes(0));
    }

    function downgradeToETH(uint wad) external override {
        ISuperToken(address(this)).selfBurn(msg.sender, wad, new bytes(0));
        msg.sender.transfer(wad);
    }

    function downgradeToWETH(IWETH weth, uint wad) external override {
        ISuperToken(address(this)).selfBurn(msg.sender, wad, new bytes(0));
        weth.deposit{ value: wad }();
        weth.transfer(msg.sender, wad);
    }

}
