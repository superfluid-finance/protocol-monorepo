// SPDX-License-Identifier: MIT
pragma solidity >= 0.5.0;

import { ISuperToken } from "./ISuperToken.sol";

interface ISuperApp {

    function beforeAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory cbdata);

    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        external
        returns (bytes memory newCtx);

    function beforeAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory cbdata);

    function afterAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata cbdata
    )
        external
        returns (bytes memory newCtx);

    function beforeAgreementTerminated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        returns (bytes memory data);

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
