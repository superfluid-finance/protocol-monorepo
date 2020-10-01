// SPDX-License-Identifier: MIT
pragma solidity >= 0.7.0;

import {
    ISuperfluid,
    ISuperToken,
    ISuperApp,
    SuperAppDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";

abstract contract SuperAppBase is ISuperApp {

    function beforeAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback - Before Agreement Created");
    }

    function afterAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Created");
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
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback - Before Agreement updated");
    }

    function afterAgreementUpdated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Updated");
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
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback -  Before Agreement Terminated");
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes memory /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Terminated");
    }

}
