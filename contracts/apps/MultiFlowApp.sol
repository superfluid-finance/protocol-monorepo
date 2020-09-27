// SPDX-License-Identifier: MIT
pragma solidity 0.7.1;

import {
    ISuperfluid,
    ISuperToken,
    SuperAppBase,
    SuperAppDefinitions
} from "./SuperAppBase.sol";
import { IConstantFlowAgreementV1 } from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

// FIXME Create and use a SuperAppBase abstract contract,
//       which implements all the callbacks as reverts.
//       - Revert(callback not implemented constant string)
//FIXME - MsgSender can be different from flowSender
contract MultiFlowsApp is SuperAppBase {

    struct ReceiverData {
        address to;
        int96 proportion;
    }

    IConstantFlowAgreementV1 internal _constantFlow;
    ISuperfluid internal _host;
    //Sender => To / Proportion
    mapping(address => ReceiverData[]) internal _userFlows;

    constructor(IConstantFlowAgreementV1 constantFlow, ISuperfluid superfluid) {
        require(address(constantFlow) != address(0), "MFA: can't set zero address as constant Flow");
        require(address(superfluid) != address(0), "MFA: can't set zero address as Superfluid");
        _constantFlow = constantFlow;
        _host = superfluid;

        uint256 configWord =
            SuperAppDefinitions.TYPE_APP_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function createMultiFlows(
        ISuperToken superToken,
        address[] calldata receivers,
        int96[] calldata proportions,
        bytes calldata ctx
    )
        external
        returns(bytes memory newCtx)
    {
        require(msg.sender == address(_host), "MFA: Only official superfluid host is supported by the app");
        require(receivers.length == proportions.length, "MFA: number receivers not equal flowRates");
        (,,address sender,,) = _host.decodeCtx(ctx);
        require(_userFlows[sender].length == 0, "MFA: Multiflow alread created");

        newCtx = _host.chargeGasFee(ctx, 30000);

        (, int256 receivingFlowRate, ,) = _constantFlow.getFlow(
            superToken,
            sender,
            address(this)
        );
        require(receivingFlowRate == 0, "MFA: Updates are not supported, go to YAM");

        for(uint256 i = 0; i < receivers.length; i++) {
            _userFlows[sender].push(ReceiverData(receivers[i], proportions[i]));
        }
    }

    function beforeAgreementUpdated(
        ISuperToken superToken,
        bytes calldata /*ctx*/,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        override
        returns (bytes memory data)
    {
        require(agreementClass == address(_constantFlow), "MFA: Unsupported agreement");
        (, int256 oldFlowRate, ,) = _constantFlow.getFlowByID(superToken, agreementId);
        return _packData(agreementId, oldFlowRate);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address /*agreementClass*/,
        bytes32 agreementId,
        bytes calldata /*cbdata*/
    )
    external
    override
    returns(bytes memory newCtx)
    {
        (,,address sender,,) = _host.decodeCtx(ctx);
        require(_userFlows[sender].length > 0 , "MFA: Create Multi Flow first or go away");
        (, int96 receivingFlowRate, , ) = _constantFlow.getFlowByID(superToken, agreementId);
        /*
        (int96 receivingFlowRate) = _constantFlow.getFlow(
            superToken,
            sender,
            address(this)
        );
        */

        require(receivingFlowRate != 0, "not zero pls");
        int96 sum = _sumProportions(_userFlows[sender]);
        newCtx = ctx;

        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            require(_userFlows[sender][i].proportion > 0, "Proportion > 0");
            require(receivingFlowRate > 0, "Receiving flow is zero");
            require(sum != 0 , "Sum is zero zero zero");
            require((int96(_userFlows[sender][i].proportion) * receivingFlowRate) / sum != 0, "SUM IS ZERO");
            (newCtx, ) = _host.callAgreementWithContext(
                _constantFlow,
                abi.encodeWithSelector(
                    _constantFlow.createFlow.selector,
                    superToken,
                    _userFlows[sender][i].to,
                    (int96(_userFlows[sender][i].proportion) * receivingFlowRate) / int96(sum),
                    new bytes(0)
                ),
                newCtx
            );
        }
    }

    function afterAgreementTerminated(
        ISuperToken superToken,
        bytes calldata ctx,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes memory /*cbdata*/
    )

        external
        override
        returns (bytes memory newCtx)
    {
        (,,address sender,,) = _host.decodeCtx(ctx);
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

    function _packData(bytes32 id, int256 flowRate) internal pure returns(bytes memory) {
        return abi.encodePacked(id, flowRate);
    }

    function _unpackData(bytes memory data) internal pure returns(bytes32, int256) {
        return abi.decode(data, (bytes32, int256));
    }

    function _sumProportions(ReceiverData[] memory receivers) internal pure returns(int96 sum) {
        for(uint256 i = 0; i < receivers.length; i++) {
            sum += receivers[i].proportion;
        }
    }
}
