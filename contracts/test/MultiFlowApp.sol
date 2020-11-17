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
 * @dev Multi Flows Super APPEND
 *
 * A super app that can split incoming flows to multiple outgoing flows.
 *
 * This is used for testing CFA callbacks logic.
 */
contract MultiFlowsApp is SuperAppBase {

    struct ReceiverData {
        address to;
        uint256 proportion;
    }

    IConstantFlowAgreementV1 internal _constantFlow;
    ISuperfluid internal _host;
    //Sender => To / Proportion
    mapping(address => ReceiverData[]) internal _userFlows;

    constructor(IConstantFlowAgreementV1 constantFlow, ISuperfluid superfluid) {
        assert(address(constantFlow) != address(0));
        assert(address(superfluid) != address(0));
        _constantFlow = constantFlow;
        _host = superfluid;

        uint256 configWord =
            SuperAppDefinitions.TYPE_APP_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function createMultiFlows(
        address[] calldata receivers,
        uint256[] calldata proportions,
        bytes calldata ctx
    )
        external
        onlyHost
        returns(bytes memory newCtx)
    {
        assert(receivers.length == proportions.length);
        (address sender,,,,) = _host.decodeCtx(ctx);

        newCtx = _host.chargeGasFee(ctx, 30000);

        delete _userFlows[sender];
        for(uint256 i = 0; i < receivers.length; i++) {
            _userFlows[sender].push(ReceiverData(receivers[i], proportions[i]));
        }
    }

    function _sumProportions(ReceiverData[] memory receivers) internal pure returns(uint256 sum) {
        for(uint256 i = 0; i < receivers.length; i++) {
            sum += receivers[i].proportion;
        }
    }

    function _updateMultiFlow(
        ISuperToken superToken,
        address sender,
        bytes4 selector,
        int96 receivingFlowRate,
        bytes calldata ctx
    )
        private
        returns (bytes memory newCtx)
    {
        uint256 sum = _sumProportions(_userFlows[sender]);

        newCtx = ctx;
        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            assert(_userFlows[sender][i].proportion > 0);
            int96 targetFlowrate = (int96(_userFlows[sender][i].proportion) * receivingFlowRate) / int96(sum);
            (newCtx, ) = _host.callAgreementWithContext(
                _constantFlow,
                abi.encodeWithSelector(
                    selector,
                    superToken,
                    _userFlows[sender][i].to,
                    targetFlowrate,
                    new bytes(0)
                ),
                newCtx
            );
        }
    }

    function beforeAgreementCreated(
        ISuperToken superToken,
        bytes calldata /*ctx*/,
        address agreementClass,
        bytes32 agreementId
    )
        external view override
        onlyHost
        returns (bytes memory cbdata)
    {
        assert(agreementClass == address(_constantFlow));
        (, int256 oldFlowRate, ,) = _constantFlow.getFlowByID(superToken, agreementId);
        return abi.encode(oldFlowRate);
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
        assert(agreementClass == address(_constantFlow));
        (address sender,,,,) = _host.decodeCtx(ctx);
        (, int96 receivingFlowRate, , ) = _constantFlow.getFlowByID(superToken, agreementId);
        newCtx = _updateMultiFlow(superToken, sender, _constantFlow.createFlow.selector, receivingFlowRate, ctx);
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
        assert(agreementClass == address(_constantFlow));
        (, int256 oldFlowRate, ,) = _constantFlow.getFlowByID(superToken, agreementId);
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
        assert(agreementClass == address(_constantFlow));
        (address sender,,,,) = _host.decodeCtx(ctx);
        (, int96 newFlowRate, , ) = _constantFlow.getFlowByID(superToken, agreementId);

        //int96 oldFlowRate = abi.decode(cbdata, (int96));
        //require(newFlowRate > oldFlowRate, "MFA: only increasing flow rate"); // Funcky logic for testing purpose

        newCtx = _updateMultiFlow(superToken, sender, _constantFlow.updateFlow.selector, newFlowRate, ctx);
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
        assert(agreementClass == address(_constantFlow));
        (address sender,,,,) = _host.decodeCtx(ctx);
        newCtx = ctx;
        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            (newCtx, ) = _host.callAgreementWithContext(
                _constantFlow,
                abi.encodeWithSelector(
                    _constantFlow.deleteFlow.selector,
                    superToken,
                    address(this),
                    _userFlows[sender][i].to,
                    new bytes(0)
                ),
                newCtx
            );
        }
        delete _userFlows[sender];
    }

    modifier onlyHost() {
        assert(msg.sender == address(_host));
        _;
    }
}
