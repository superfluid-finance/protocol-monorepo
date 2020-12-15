// SPDX-License-Identifier: MIT
pragma solidity 0.7.5;

import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "../apps/SuperAppBase.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";


/**
 * @dev Multi Flow (Super) App
 *
 * A super app that can split incoming flows to multiple outgoing flows.
 *
 * This is used for testing CFA callbacks logic.
 */
contract MultiFlowApp is SuperAppBase {

    struct ReceiverData {
        address to;
        uint256 proportion;
    }

    struct Configuration {
        uint8 ratioPct;
        ReceiverData[] receivers;
    }

    IConstantFlowAgreementV1 internal _cfa;
    ISuperfluid internal _host;

    constructor(IConstantFlowAgreementV1 cfa, ISuperfluid superfluid) {
        assert(address(cfa) != address(0));
        assert(address(superfluid) != address(0));
        _cfa = cfa;
        _host = superfluid;

        uint256 configWord =
            SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function _parseUserData(
        bytes memory userData
    )
        private pure
        returns (Configuration memory configuration)
    {
        // parse user data
        uint8 ratioPct;
        address[] memory receivers;
        uint256[] memory proportions;
        (ratioPct, receivers, proportions) = abi.decode(userData, (uint8, address[], uint256[]));
        assert(receivers.length == proportions.length);

        configuration.ratioPct = ratioPct;
        configuration.receivers = new ReceiverData[](receivers.length);
        for(uint256 i = 0; i < receivers.length; i++) {
            assert(proportions[i] > 0);
            configuration.receivers[i] = ReceiverData(receivers[i], proportions[i]);
        }
    }

    function _sumProportions(ReceiverData[] memory receivers) internal pure returns(uint256 sum) {
        for(uint256 i = 0; i < receivers.length; i++) {
            sum += receivers[i].proportion;
        }
    }

    function _updateMultiFlow(
        Configuration memory configuration,
        ISuperToken superToken,
        bytes4 selector,
        int96 flowRate,
        uint256 appAllowance,
        bytes calldata ctx
    )
        private
        returns (bytes memory newCtx)
    {
        uint256 sum = _sumProportions(configuration.receivers);

        newCtx = ctx;

        // scale the flow rate and app allowance numbers
        flowRate = flowRate * configuration.ratioPct / 100;
        appAllowance = appAllowance * configuration.ratioPct / 100;

        for(uint256 i = 0; i < configuration.receivers.length; i++) {
            bytes memory callData;
            {
                ReceiverData memory receiverData = configuration.receivers[i];
                uint256 targetAllowance = appAllowance * receiverData.proportion / sum;
                int96 targetFlowRate = _cfa.getMaximumFlowRateFromDeposit(
                    superToken,
                    targetAllowance
                );
                flowRate -= targetFlowRate;
                callData = abi.encodeWithSelector(
                    selector,
                    superToken,
                    receiverData.to,
                    targetFlowRate,
                    new bytes(0)
                );
            }
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                callData,
                new bytes(0), // user data
                newCtx
            );
        }
        assert(flowRate >= 0);
    }

    // solhint-disable-next-line contract-name-camelcase
    struct _StackVars {
        int96 flowRate;
        uint256 appAllowance;
        bytes userData;
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns(bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        _StackVars memory vars;
        Configuration memory configuration;
        (,,,,vars.userData,vars.appAllowance,,) = _host.decodeCtx(ctx);
        (,vars.flowRate,,) = _cfa.getFlowByID(superToken, agreementId);
        assert(vars.appAllowance > 0);
        configuration = _parseUserData(vars.userData);
        newCtx = _updateMultiFlow(
            configuration,
            superToken,
            _cfa.createFlow.selector,
            vars.flowRate,
            vars.appAllowance,
            ctx);
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*agreementData*/,
        bytes calldata /*ctx*/
    )
        external view override
        onlyHost
        returns (bytes memory cbdata)
    {
        assert(agreementClass == address(_cfa));
        (, int256 oldFlowRate, ,) = _cfa.getFlowByID(superToken, agreementId);
        return abi.encode(oldFlowRate);
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*agreementData*/,
        bytes calldata /* cbdata */,
        bytes calldata ctx
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        _StackVars memory vars;
        Configuration memory configuration;
        (,,,,vars.userData,vars.appAllowance,,) = _host.decodeCtx(ctx);
        (,vars.flowRate,,) = _cfa.getFlowByID(superToken, agreementId);
        assert(vars.appAllowance > 0);
        configuration = _parseUserData(vars.userData);
        newCtx = _updateMultiFlow(
            configuration,
            superToken,
            _cfa.updateFlow.selector,
            vars.flowRate,
            vars.appAllowance,
            ctx);
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )

        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        _StackVars memory vars;
        Configuration memory configuration;
        (,,,,vars.userData,,,) = _host.decodeCtx(ctx);
        configuration = _parseUserData(vars.userData);
        newCtx = ctx;
        for(uint256 i = 0; i < configuration.receivers.length; i++) {
            bytes memory callData = abi.encodeWithSelector(
                _cfa.deleteFlow.selector,
                superToken,
                address(this),
                configuration.receivers[i].to,
                new bytes(0) //placeholder ctx
            );
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                callData,
                new bytes(0), // user data
                newCtx
            );
        }
    }

    modifier onlyHost() {
        assert(msg.sender == address(_host));
        _;
    }
}
