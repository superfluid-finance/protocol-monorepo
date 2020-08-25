// SPDX-License-Identifier: MIT
pragma solidity >=0.7.0;

import "../interface/AppHelper.sol";
import "../interface/ISuperAppBase.sol";
import "../interface/IFlowAgreement.sol";
import "../interface/ISuperfluid.sol";
import { ContextLibrary } from "../interface/ContextLibrary.sol";

// FIXME Create and use a SuperAppBase abstract contract,
//       which implements all the callbacks as reverts.
//       - Revert(callback not implemented constant string)
contract MultiFlowsApp is ISuperAppBase {

    struct ReceiverData {
        address to;
        uint256 proportion;
    }

    IFlowAgreement internal _constantFlow;
    ISuperfluid internal _host;
    //Sender => To / Proportion
    mapping(address => ReceiverData[]) internal _userFlows;

    constructor(IFlowAgreement constantFlow, ISuperfluid superfluid) public {
        require(address(constantFlow) != address(0), "SA: can't set zero address as constant Flow");
        require(address(superfluid) != address(0), "SA: can't set zero address as Superfluid");
        _constantFlow = constantFlow;
        _host = superfluid;

        uint256 configWord =
            AppHelper.TYPE_APP_FINAL |
            AppHelper.BEFORE_AGREEMENT_CREATED_NOOP |
            AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP;

        _host.registerApp(configWord);
    }

    function createMultiFlows(
        ISuperToken superToken,
        address[] calldata receivers,
        uint256[] calldata proportions,
        bytes calldata ctx
    )
        external
    {
        require(msg.sender == address(_host), "Only official superfluid host is supported by the app");
        require(receivers.length == proportions.length, "MFA: number receivers not equal flowRates");
        address sender = ContextLibrary.getCaller(ctx);
        require(_userFlows[sender].length == 0, "MFA: Multiflow alread created");

        _host.chargeGasFee(30000);

        (int256 receivingFlowRate) = _constantFlow.getFlow(
            superToken,
            sender,
            address(this)
        );
        require(receivingFlowRate == 0, "MAPP: Updates are not supported, go to YAM");

        //uint256 sum = _sumProportions(proportions);
        for(uint256 i = 0; i < receivers.length; i++) {
            _userFlows[sender].push(ReceiverData(receivers[i], proportions[i]));
        }
        //_appFlows[address(this)][sender] = totalOutFlowRate;
        //require(totalOutFlowRate <= receivingFlowRate, "MultiApp: Receiving flow don't cover the costs");
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
        require(agreementClass == address(_constantFlow), "Unsupported agreement");
        (, address sender, , int256 oldFlowRate) = _constantFlow.getFlow(superToken, agreementId);
        return _packData(sender, oldFlowRate);
    }

    function afterAgreementCreated(
        ISuperToken superToken,
        bytes calldata ctx,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*cbdata*/
    )
    external
    override
    returns(bytes memory newCtx)
    {
        address sender = ContextLibrary.getCaller(ctx);
        require(_userFlows[sender].length > 0 , "MAPP: Create Multi Flow first or go away");
        (int256 receivingFlowRate) = _constantFlow.getFlow(
            superToken,
            sender,
            address(this)
        );
        uint256 sum = _sumProportions(_userFlows[sender]);
        newCtx = ctx;

        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            (newCtx, ) = _host.callAgreementWithContext(
                address(_constantFlow),
                abi.encodeWithSelector(
                    _constantFlow.createFlow.selector,
                    superToken,
                    _userFlows[sender][i].to,
                    (int256(_userFlows[sender][i].proportion) * receivingFlowRate) / int256(sum),
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
        (, address sender, ) = ContextLibrary.decodeContext(ctx);
        newCtx = ctx;
        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            (newCtx, ) = _host.callAgreementWithContext(
                address(_constantFlow),
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

    function _packData(address account, int256 flowRate) internal pure returns(bytes memory) {
        return abi.encodePacked(account, flowRate);
    }

    function _unpackData(bytes memory data) internal pure returns(address, int256) {
        return abi.decode(data, (address, int256));
    }

    function _sumProportions(ReceiverData[] memory receivers) internal returns(uint256) {
        uint256 sum;
        for(uint256 i = 0; i < receivers.length; i++) {
            sum += receivers[i].proportion;
        }
        return sum;
    }
}
