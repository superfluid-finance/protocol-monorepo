// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import "./ISuperToken.sol";

/**
 * @title Superfluid's Governance interface
 * @author Superfluid
 */
interface ISuperfluidGovernance {

    /// @notice Get the Reward address that receives the liquidation fees.
    /// @param underlying Token address.
    function getRewardAddress(
        address underlying
    )
        external
        view
        returns(address rewardAddress);

    /// @notice Get the Minimal Balance value that one account should always maintain
    /// @param underlying Token address.
    function getMinimalDeposit(
        address underlying
    )
        external
        view
        returns(uint16 minimalBalance);

    /// @notice Get the Period that is allowed to perform a liquidation
    /// @param underlying Token address.
    function getLiquidationPeriod(
        address underlying
    )
        external
        view
        returns(uint16 period);

    function getMaxGasCallback() external view returns(uint64);

    function getMaxGasApp() external view returns(uint64);

    function getSuperfluid()
        external
        view
        returns(address);
}
