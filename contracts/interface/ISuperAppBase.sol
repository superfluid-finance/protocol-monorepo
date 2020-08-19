// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import "./ISuperToken.sol";

abstract contract ISuperAppBase {

    function beforeAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        returns (bytes memory /*data*/)
    {
        revert("Unsupported callback");
    }

    function afterAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*data*/
    )
        external
        virtual
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback");
    }

    function beforeAgreementUpdated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        returns (bytes memory /*data*/)
    {
        revert("Unsupported callback");
    }

    function afterAgreementUpdated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*data*/
    )
        external
        virtual
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback");
    }

    function beforeAgreementTerminated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        returns (bytes memory /*data*/)
    {
        revert("Unsupported callback");
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        virtual
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback");
    }
}
