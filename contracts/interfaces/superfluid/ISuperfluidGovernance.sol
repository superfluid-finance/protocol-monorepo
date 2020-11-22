// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { ISuperAgreement } from "./ISuperAgreement.sol";
import { ISuperfluidToken } from "./ISuperToken.sol";


/**
 * @dev Superfluid's Governance interface
 *
 * @author Superfluid
 */
interface ISuperfluidGovernance {

    function replaceGovernance(address host, ISuperfluidGovernance newGov) external;

    function setSuperTokenLogic(address host, address logic) external;

    function registerAgreementClass(address host, ISuperAgreement agreementClass) external;

    function updateAgreementClass(address host, ISuperAgreement agreementClass) external;

    /**
     * @dev Get the Reward address that receives the liquidation fees.
     * @param superToken Super token address.
     */
    function getRewardAddress(
        ISuperfluidToken superToken
    )
        external
        view
        returns(address rewardAddress);

    /**
     * @dev Get the Period that is allowed to perform a liquidation
     * @param superToken Super token address.
     */
    function getLiquidationPeriod(
        ISuperfluidToken superToken
    )
        external
        view
        returns(uint256 period);

}
