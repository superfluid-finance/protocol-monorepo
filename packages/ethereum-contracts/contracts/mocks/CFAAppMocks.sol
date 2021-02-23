// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma abicoder v2;


import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "../apps/SuperAppBase.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";


/**
 * @dev This is CFA SuperApp that maintains at most one inflw from a sender at any moment.
 *
 * This can test the deposit allowance logic in the deleteFlow as a recipient.
 */
contract ExclusiveInflowTestApp is SuperAppBase {

    IConstantFlowAgreementV1 private _cfa;
    ISuperfluid private _host;
    address private _currentSender;

    constructor(IConstantFlowAgreementV1 cfa, ISuperfluid superfluid) {
        assert(address(cfa) != address(0));
        assert(address(superfluid) != address(0));
        _cfa = cfa;
        _host = superfluid;

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL
            | SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP
            // | SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP
            | SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP
            | SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP
            | SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP
            // | SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP
            ;

        _host.registerApp(configWord);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns(bytes memory newCtx)
    {
        newCtx = ctx;
        ISuperfluid.Context memory context = _host.decodeCtx(ctx);
        if (_currentSender != address(0)) {
            bytes memory callData = abi.encodeWithSelector(
                _cfa.deleteFlow.selector,
                superToken,
                _currentSender,
                address(this),
                new bytes(0)
            );
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                callData,
                new bytes(0), // user data
                newCtx
            );
        }
        _currentSender = context.msgSender;
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata agreementData,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns(bytes memory newCtx)
    {
        (address flowSender, ) = abi.decode(agreementData, (address, address));
        assert(flowSender == _currentSender);
        _currentSender = address(0);
        return ctx;
    }

    modifier onlyHost() {
        assert(msg.sender == address(_host));
        _;
    }

}
