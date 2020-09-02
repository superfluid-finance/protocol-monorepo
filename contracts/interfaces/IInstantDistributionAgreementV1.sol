// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import "./ISuperToken.sol";
import "./ISuperAgreement.sol";

/**
 * @title Superfluid's flow agreement interface
 * @author Superfluid
 */
abstract contract IInstantDistributionAgreementV1 is ISuperAgreement {

    function createIndex(
        ISuperToken token,
        uint256 indexId,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function updateIndex(
        ISuperToken token,
        uint256 indexId,
        uint256 indexValue,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

    function createSubscription(
        ISuperToken token,
        address receiver,
        uint256 units,
        bytes calldata ctx)
            external
            virtual
            returns(bytes memory newCtx);

}
