// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import "@openzeppelin/contracts/access/AccessControl.sol";
import {
    ISuperToken,
    IERC20
} from "../interfaces/superfluid/ISuperfluid.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";

contract SuperUpgrader is AccessControl {
    // Create a new role identifier for the backend role
    bytes32 public constant BACKEND_ROLE = keccak256("BACKEND_ROLE");

    using SafeMath for uint256;

    constructor(address adminRole, address[] memory backendAddr) {
        require(adminRole != address(0), "adminRole is empty");
        _setupRole(DEFAULT_ADMIN_ROLE, adminRole);
        for (uint256 i = 0; i < backendAddr.length; ++i) {
            require(backendAddr[i] != address(0), "backend can't be zero");
            _setupRole(BACKEND_ROLE, backendAddr[i]);
        }
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
    {
        require(msg.sender == account || hasRole(BACKEND_ROLE, msg.sender), "operation not allowed");
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
}
