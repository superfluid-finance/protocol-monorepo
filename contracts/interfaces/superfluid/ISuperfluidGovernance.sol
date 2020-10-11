// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;


/**
 * @dev Superfluid's Governance interface
 *
 * @author Superfluid
 */
interface ISuperfluidGovernance {

    function addAgreement(address host, address agreementClass) external;

    /**
     * @dev Get the Reward address that receives the liquidation fees.
     * @param superToken Super token address.
     */
    function getRewardAddress(
        address superToken
    )
        external
        view
        returns(address rewardAddress);

    /**
     * @dev Get the Period that is allowed to perform a liquidation
     * @param superToken Super token address.
     */
    function getLiquidationPeriod(
        address superToken
    )
        external
        view
        returns(uint256 period);

}
