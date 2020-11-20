// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

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
    //Sender => To / Proportion
    mapping(address => Configuration) internal _userConfigs;

    constructor(IConstantFlowAgreementV1 cfa, ISuperfluid superfluid) {
        assert(address(cfa) != address(0));
        assert(address(superfluid) != address(0));
        _cfa = cfa;
        _host = superfluid;

        uint256 configWord =
            SuperAppDefinitions.TYPE_APP_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function createMultiFlows(
        uint8 ratioPct,
        address[] calldata receivers,
        uint256[] calldata proportions,
        bytes calldata ctx
    )
        external
        onlyHost
        returns(bytes memory newCtx)
    {
        assert(receivers.length == proportions.length);
        (,,address sender,,,) = _host.decodeCtx(ctx);

        newCtx = _host.chargeGasFee(ctx, 30000);

        _userConfigs[sender].ratioPct = ratioPct;
        delete _userConfigs[sender].receivers;
        for(uint256 i = 0; i < receivers.length; i++) {
            assert(proportions[i] > 0);
            _userConfigs[sender].receivers.push(ReceiverData(receivers[i], proportions[i]));
        }
    }

    function _sumProportions(ReceiverData[] memory receivers) internal pure returns(uint256 sum) {
        for(uint256 i = 0; i < receivers.length; i++) {
            sum += receivers[i].proportion;
        }
    }

    function _updateMultiFlow(
        ISuperToken superToken,
        bytes4 selector,
        address sender,
        int96 flowRate,
        uint256 appAllowance,
        bytes calldata ctx
    )
        private
        returns (bytes memory newCtx)
    {
        uint256 sum = _sumProportions(_userConfigs[sender].receivers);

        newCtx = ctx;

        // scale the flow rate and app allowance numbers
        flowRate = flowRate * _userConfigs[sender].ratioPct / 100;
        appAllowance = appAllowance * _userConfigs[sender].ratioPct / 100;

        for(uint256 i = 0; i < _userConfigs[sender].receivers.length; i++) {
            bytes memory callData;
            {
                ReceiverData memory receiverData = _userConfigs[sender].receivers[i];
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
                newCtx
            );
        }
        assert(flowRate >= 0);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /*cbdata*/
    )
        external override
        onlyHost
        returns(bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        address sender;
        int96 flowRate;
        int256 appAllowance;
        (,,sender,,appAllowance,) = _host.decodeCtx(ctx);
        (,flowRate,,) = _cfa.getFlowByID(superToken, agreementId);
        assert(appAllowance > 0);
        newCtx = _updateMultiFlow(
            superToken,
            _cfa.createFlow.selector,
            sender,
            flowRate,
            uint256(appAllowance),
            ctx);
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        bytes calldata /*ctx*/,
        address agreementClass,
        bytes32 agreementId
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
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata /* cbdata */
    )
        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        address sender;
        int96 flowRate;
        int256 appAllowance;
        (,,sender,,appAllowance,) = _host.decodeCtx(ctx);
        (,flowRate,,) = _cfa.getFlowByID(superToken, agreementId);
        assert(appAllowance > 0);
        newCtx = _updateMultiFlow(
            superToken,
            _cfa.updateFlow.selector,
            sender,
            flowRate,
            uint256(appAllowance),
            ctx);
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 /*agreementId*/,
        bytes memory /*cbdata*/
    )

        external override
        onlyHost
        returns (bytes memory newCtx)
    {
        assert(agreementClass == address(_cfa));
        (,,address sender,,,) = _host.decodeCtx(ctx);
        newCtx = ctx;
        for(uint256 i = 0; i < _userConfigs[sender].receivers.length; i++) {
            (newCtx, ) = _host.callAgreementWithContext(
                _cfa,
                abi.encodeWithSelector(
                    _cfa.deleteFlow.selector,
                    superToken,
                    address(this),
                    _userConfigs[sender].receivers[i].to,
                    new bytes(0)
                ),
                newCtx
            );
        }
        delete _userConfigs[sender];
    }

    modifier onlyHost() {
        assert(msg.sender == address(_host));
        _;
    }
}
