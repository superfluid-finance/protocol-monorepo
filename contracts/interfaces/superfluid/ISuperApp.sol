// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import { ISuperToken } from "./ISuperToken.sol";


/**
 * @title Superfluid's app interface.
 *
 * NOTE:
 * - Be fearful of the app jail, when the word permitted is used.
 *
 * @author Superfluid
 */
interface ISuperApp {

    /**
     * @dev Callback before a new agreement is created.
     * @param superToken The super token used for the agreement.
     * @param ctx The context data.
     * @param agreementClass The agreement class address.
     * @param agreementId The agreementId
     * @return cbdata A free format in memory data the app can use to pass
     *          arbitary information to the after-hook callback.
     *
     * NOTE:
     * - It will be invoked with `staticcall`, no state changes are permitted.
     * - Only revert with a "reason" is permitted.
     */
    function beforeAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory cbdata);

    /**
     * @dev Callback after a new agreement is created.
     * @param superToken The super token used for the agreement.
     * @param ctx The context data.
     * @param agreementClass The agreement class address.
     * @param agreementId The agreementId
     * @param cbdata The data returned from the before-hook callback.
     * @return newCtx The current context of the transaction.
     *
     * NOTE:
     * - State changes is permitted.
     * - Only revert with a "reason" is permitted.
     */
    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        external
        returns (bytes memory newCtx);

    /**
     * @dev Callback before a new agreement is updated.
     * @param superToken The super token used for the agreement.
     * @param ctx The context data.
     * @param agreementClass The agreement class address.
     * @param agreementId The agreementId
     * @return cbdata A free format in memory data the app can use to pass
     *          arbitary information to the after-hook callback.
     *
     * NOTE:
     * - It will be invoked with `staticcall`, no state changes are permitted.
     * - Only revert with a "reason" is permitted.
     */
    function beforeAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory cbdata);


    /**
    * @dev Callback after a new agreement is updated.
    * @param superToken The super token used for the agreement.
    * @param ctx The context data.
    * @param agreementClass The agreement class address.
    * @param agreementId The agreementId
    * @param cbdata The data returned from the before-hook callback.
    * @return newCtx The current context of the transaction.
    *
    * NOTE:
    * - State changes is permitted.
    * - Only revert with a "reason" is permitted.
    */
    function afterAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        external
        returns (bytes memory newCtx);

    /**
    * @dev Callback before a new agreement is terminated.
    * @param superToken The super token used for the agreement.
    * @param ctx The context data.
    * @param agreementClass The agreement class address.
    * @param agreementId The agreementId
    * @return cbdata A free format in memory data the app can use to pass
    *          arbitary information to the after-hook callback.
    *
    * NOTE:
    * - It will be invoked with `staticcall`, no state changes are permitted.
    * - Revert is not permitted.
    */
    function beforeAgreementTerminated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory cbdata);

    /**
    * @dev Callback after a new agreement is terminated.
    * @param superToken The super token used for the agreement.
    * @param ctx The context data.
    * @param agreementClass The agreement class address.
    * @param agreementId The agreementId
    * @param cbdata The data returned from the before-hook callback.
    * @return newCtx The current context of the transaction.
    *
    * NOTE:
    * - State changes is permitted.
    * - Revert is not permitted.
    */
    function afterAgreementTerminated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        external
        returns (bytes memory newCtx);
}
