// SPDX-License-Identifier: AGPLv3
pragma solidity ^0.8.13;

import {IUniswapV2Router02} from "@uniswap/v2-periphery/contracts/interfaces/IUniswapV2Router02.sol";
import {IERC20} from "@uniswap/v2-periphery/contracts/interfaces/IERC20.sol";
import {
    ISuperfluid,
    IInstantDistributionAgreementV1,
    IConstantFlowAgreementV1,
    StreamInDistributeOut,
    ISuperToken
} from "./base/StreamInDistributeOut.sol";

/// @title Contract to Stream in, Swap, then Distribute out.
contract StreamSwapDistribute is StreamInDistributeOut {

    /// @dev Uniswap V2 Router for swapping tokens.
    IUniswapV2Router02 internal immutable _router;

    constructor(
        ISuperfluid host,
        IConstantFlowAgreementV1 cfa,
        IInstantDistributionAgreementV1 ida,
        ISuperToken inToken,
        ISuperToken outToken,
        IUniswapV2Router02 router
    ) StreamInDistributeOut(host, cfa, ida, inToken, outToken) {
        _router = router;

        // approve router to transfer the underlying `inToken` on behalf of this contract
        IERC20(inToken.getUnderlyingToken()).approve(address(router), type(uint256).max);

        // approve `outToken` to upgrade the underlying `outToken` on behalf of this contract.
        IERC20(outToken.getUnderlyingToken()).approve(address(outToken), type(uint256).max);
    }

    /// @dev Before action callback. This swaps the `inToken` for the `outToken`, then returns the
    /// amount to distribute out in the `executeAction` function.
    /// @return distributionAmount amount to distribute after the callback.
    function _beforeDistribution() internal override returns (uint256 distributionAmount) {
        // Downgrade the full balance of the `_inToken`.
        _inToken.downgrade(_inToken.balanceOf(address(this)));

        // Get the underlying address of the `_inToken`.
        address inTokenUnderlying = _inToken.getUnderlyingToken();

        // Get the amount of `_inToken`s to swap
        uint256 amountIn = IERC20(inTokenUnderlying).balanceOf(address(this));

        // Create the `path` of swaps for the Uniswap Router.
        address[] memory path = new address[](2);
        path[0] = inTokenUnderlying;
        path[1] = _outToken.getUnderlyingToken();

        // Swap the full balance of underlying `_inToken` for the underlying `_outToken`.
        // Set the deadline for 1 minute into the future.
        _router.swapExactTokensForTokens(
            amountIn,
            0,
            path,
            address(this),
            type(uint256).max
        );

        // Get the full balance of the underlying `_outToken`.
        // Implicitly return the `upgrade`d amount by the end of the function.
        distributionAmount = IERC20(_outToken.getUnderlyingToken()).balanceOf(address(this));

        //Upgrade the full underlying `_outToken` balance.
        _outToken.upgrade(distributionAmount);
    }
}
