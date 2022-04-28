// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.13;

import { ISuperToken, IERC20 } from "../superfluid/Superfluid.sol";

contract MockSmartWallet {

    function approveTest(IERC20 token, address spender, uint256 amount) external {
        token.approve(spender, amount);
    }

    function upgradeToTest(ISuperToken superToken, address to, uint256 amount, bytes calldata data) external {
        superToken.upgradeTo(to, amount, data);
    }
}