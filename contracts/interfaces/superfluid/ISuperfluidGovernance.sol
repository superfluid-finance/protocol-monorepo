// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;


/**
 * @dev Superfluid's Governance interface
 *
 * @author Superfluid
 */
interface ISuperfluidGovernance {

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

    /**
     * @dev Add an agreement to the whitelist
     */
    function addAgreement(address agreementClass) external;

    /**
     * @dev Check if the agreement is whitelisted
     */
    function isAgreementListed(address agreementClass)
        external view
        returns(bool yes);

    /**
     * @dev Map list of the agreements using a bitmap
     * @param bitmap Agreement class ID based bitmap
     */
    function mapAgreements(uint256 bitmap)
        external view
        returns (address[] memory agreementClasses);

    /**
     * @dev Create a new bitmask by adding agreement class to it.
     */
    function maskAgreementBit(uint256 bitmap, address agreementClass)
        external view
        returns (uint256 newBitmap);

    /**
     * @dev Create a new bitmask by removing agreement class from it.
     */
    function unmaskAgreementBit(uint256 bitmap, address agreementClass)
        external view
        returns (uint256 newBitmap);

}
