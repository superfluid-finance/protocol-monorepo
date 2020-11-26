// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import { Ownable } from "../../access/Ownable.sol";
import {
    ISuperToken,
    IERC20
} from "../../interfaces/superfluid/ISuperfluid.sol";
import { IERC777Recipient } from "@openzeppelin/contracts/token/ERC777/IERC777Recipient.sol";
import { IERC777Sender } from "@openzeppelin/contracts/token/ERC777/IERC777Sender.sol";
import "@openzeppelin/contracts/introspection/IERC1820Registry.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";

contract SuperUpgrader is Ownable {

    using SafeMath for uint256;

    IERC1820Registry private _erc1820 = IERC1820Registry(0x1820a4B7618BdE71Dce8cdc73aAB6C95905faD24);
    bytes32 constant private _TOKENS_RECIPIENT_INTERFACE_HASH = keccak256("ERC777TokensRecipient");

    constructor(address backendAddr) {
        require(backendAddr != address(0), "Backend Address can't be zero");
        _owner = backendAddr;
        _erc1820.setInterfaceImplementer(address(this), _TOKENS_RECIPIENT_INTERFACE_HASH, address(this));
    }

    /**
     * @notice The user should ERC20.approve this contract.
     * @dev Execute upgrade function in the name of the user
     * @param superTokenAddr Super Token Address to upgrade 
     * @param account User address that previous approved this contract.
     * @param amount Amount value to be upgraded.
     */
    function upgrade(
        address superTokenAddr,
        address account,
        uint256 amount
    )
    external
    onlyOwner
    {
        //get underlaying token
        ISuperToken superToken = ISuperToken(superTokenAddr);
        uint256 oldBalance = superToken.balanceOf(address(this));
        //get tokens from user
        IERC20 token = IERC20(superToken.getUnderlyingToken());
        token.transferFrom(account, address(this), amount);
        token.approve(address(superToken), amount);
        //upgrade tokens to SuperTokens
        superToken.upgrade(amount);
        uint256 newBalance = superToken.balanceOf(address(this));
        //SuperTokens back to user
        superToken.transfer(account, newBalance.sub(oldBalance));
    }

    function tokensReceived(
        address,
        address from,
        address,
        uint256 amount,
        bytes calldata userData,
        bytes calldata
    )
        external
    {

    }
}
