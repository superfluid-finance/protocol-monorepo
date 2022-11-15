// SPDX-License-Identifier: AGPLv3
pragma solidity >= 0.8.0;

import { ISuperToken } from "../superfluid/ISuperToken.sol";


/**
 * @title Super ETH (SETH) custom token interface
 * @author Superfluid
 */
interface ISETHCustom {
    // using native token
    function upgradeByETH() external payable;
    function upgradeByETHTo(address to) external payable;
    function downgradeToETH(uint wad) external;

    // using wrapped native token
    function getUnderlyingToken() external view returns(address tokenAddr);
    function upgrade(uint256 amount) external;
    function upgradeTo(address to, uint256 amount, bytes calldata data) external;
    function downgrade(uint256 amount) external;
}

/**
 * @title Super ETH (SETH) full interface
 * @author Superfluid
 */
// solhint-disable-next-line no-empty-blocks
interface ISETH is ISETHCustom, ISuperToken {
    function getUnderlyingToken() external override(ISETHCustom, ISuperToken) view returns(address tokenAddr);
    function upgrade(uint256 amount) external override(ISETHCustom, ISuperToken);
    function upgradeTo(address to, uint256 amount, bytes calldata data) external override(ISETHCustom, ISuperToken);
    function downgrade(uint256 amount) external override(ISETHCustom, ISuperToken);
}
