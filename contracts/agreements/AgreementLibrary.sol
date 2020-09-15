// SPDX-License-Identifier: MIT
pragma solidity 0.7.0;

import "../interfaces/ISuperfluid.sol";
import "../interfaces/ISuperApp.sol";
import "../superfluid/SuperAppDefinitions.sol";

library AgreementLibrary {

    function _beforeAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        private
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        newCtx = ctx;
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(account);


        if (isSuperApp &&
            ((configWord & noopBit) == 0)) {
            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                ctx,
                agreementClass,
                agreementId
            );
            (cbdata, newCtx) = host.callAppBeforeCallback(account, data, ctx);
        }
    }

    function _afterAgreement(
        bytes4 selector,
        uint256 noopBit,
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        private
        returns(bytes memory newCtx)
    {
        (bool isSuperApp, uint256 configWord) =
            host.getAppManifest(account);


        if (isSuperApp &&
            ((configWord & noopBit) == 0)) {
            bytes memory data = abi.encodeWithSelector(
                selector,
                token,
                ctx,
                agreementClass,
                agreementId,
                cbdata
            );
            return host.callAppAfterCallback(account, data, ctx);
        }

        return ctx;
    }

    function beforeAgreementCreated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementCreated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId
        );
    }

    function afterAgreementCreated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {

        return _afterAgreement(
            ISuperApp.afterAgreementCreated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata
        );
    }

    function beforeAgreementUpdated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementUpdated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId
        );
    }

    function afterAgreementUpdated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {
        return _afterAgreement(
            ISuperApp.afterAgreementUpdated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata
        );
    }

    function beforeAgreementTerminated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId
    )
        internal
        returns(bytes memory cbdata, bytes memory newCtx)
    {
        return _beforeAgreement(
            ISuperApp.beforeAgreementTerminated.selector,
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId
        );
    }

    function afterAgreementTerminated(
        ISuperfluid host,
        ISuperToken token,
        bytes memory ctx,
        address agreementClass,
        address account,
        bytes32 agreementId,
        bytes memory cbdata
    )
        internal
        returns(bytes memory newCtx)
    {
        return _afterAgreement(
            ISuperApp.afterAgreementTerminated.selector,
            SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP,
            host,
            token,
            ctx,
            agreementClass,
            account,
            agreementId,
            cbdata
        );
    }
}
