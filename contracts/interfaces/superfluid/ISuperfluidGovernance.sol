// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { ISuperAgreement } from "./ISuperAgreement.sol";
import { ISuperfluidToken } from "./ISuperToken.sol";
import { ISuperfluid } from "./ISuperfluid.sol";


/**
 * @dev Superfluid's Governance interface
 *
 * @author Superfluid
 */
interface ISuperfluidGovernance {

    function updateHostCode(ISuperfluid host, address newCode) external;

    function replaceGovernance(ISuperfluid host, address newGov) external;

    function setSuperTokenLogic(ISuperfluid host, address newLogic) external;

    function registerAgreementClass(ISuperfluid host, address agreementClass) external;

    function updateAgreementClass(ISuperfluid host, address agreementClass) external;

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
