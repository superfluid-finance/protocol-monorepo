// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma abicoder v2;

import { ISuperToken, IERC20 } from "../superfluid/Superfluid.sol";

contract MockSmartWallet {

    function approveTest(IERC20 token, address spender, uint256 amount) external {
        token.approve(spender, amount);
    }

    function upgradeToTest(ISuperToken superToken, address to, uint256 amount, bytes calldata data) external {
        superToken.upgradeTo(to, amount, data);
    }
}