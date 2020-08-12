// SPDX-License-Identifier: MIT
/* solhint-disable*/
pragma solidity ^0.6.0;

import "../interface/AppHelper.sol";
import "../interface/ISuperApp.sol";
import "../interface/IFlowAgreement.sol";
import "../interface/ISuperfluid.sol";

contract MultiFlowsApp is ISuperApp {

    struct ReceiverData {
        address to;
        int256 flowRate;
    }

    IFlowAgreement internal _constantFlow;
    mapping(address => ReceiverData[]) internal _userFlows;
    mapping(address => mapping(address => int256)) internal _appFlows;

    constructor(IFlowAgreement constantFlow, ISuperfluid superfluid) public {
        require(address(constantFlow) != address(0), "SA: can't set zero address as constant Flow");
        _constantFlow = constantFlow;

        superfluid.registerSuperApp(
            AppHelper.TYPE_APP_FINAL |
            AppHelper.BEFORE_AGREEMENT_CREATED_NOOP |
            AppHelper.AFTER_AGREEMENT_CREATED_NOOP |
            AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP
        );
    }

    /*
    function implementationBitmask() external override view returns(uint) {
        return AppHelper.BEFORE_AGREEMENT_CREATED_NOOP |
            AppHelper.AFTER_AGREEMENT_CREATED_NOOP |
            AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP;
    }
    */

    function createMultiFlows(
        ISuperToken superToken,
        address[] calldata receivers,
        int256[] calldata flowRates
    )
        external
    {
        /*
            The receiving flow (msg.sender -> address(this)) if greater then the total flows?
            Foreach receiver should check if there is another flow running. If so then we should update the flowRate
        */
        require(receivers.length == flowRates.length, "SA: number receivers not equal flowRates");
        (, , ,int256 receivingFlowRate) = _constantFlow.getFlow(
            superToken,
            keccak256(abi.encodePacked(
                msg.sender, address(this)
            )));
        int256 totalOutFlowRate;
        //TODO: msg.sender
        int256 existingFlowRate = _appFlows[address(this)][msg.sender];
        for(uint256 i = 0; i < receivers.length; i++) {
            totalOutFlowRate += flowRates[i];
            if(existingFlowRate == 0) {
                _constantFlow.createFlow(superToken, address(this), receivers[i], flowRates[i]);
            } else {
                _constantFlow.updateFlow(superToken, address(this), receivers[i], flowRates[i]);
            }
            _userFlows[msg.sender].push(ReceiverData(receivers[i], flowRates[i]));
        }
        _appFlows[address(this)][msg.sender] = totalOutFlowRate;

        require(totalOutFlowRate <= receivingFlowRate, "MultiApp: Receiving flow don't cover the costs");
    }

    function beforeAgreementCreated(
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        override
    {
        revert("Unsupported callback");
    }

    function afterAgreementCreated(
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata data
    )
        external
        override
    {
        revert("Unsupported callback");
    }

    function beforeAgreementUpdated(
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        override
        returns (bytes memory data)
    {
        require(agreementClass == address(_constantFlow), "Unsupported agreement");
        (, address sender, , int256 oldFlowRate) = _constantFlow.getFlow(superTokenAddr, agreementId);
        return _pack(sender, oldFlowRate);
    }

    function afterAgreementUpdated(
        ISuperToken superToken,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata data
    )
    external
    override
    {
        (address sender, int256 oldFlowRate) = _unpack(data);
        (, , , int256 newFlowRate) = _constantFlow.getFlow(superToken, agreementId);
        int256 scaleFactor = newFlowRate / oldFlowRate;
        int256 totalOutFlowRate;
        for(uint256 i = 0; i < _userFlows[sender].length; i++) {
            _userFlows[sender][i].flowRate *= scaleFactor;
            totalOutFlowRate += _userFlows[sender][i].flowRate;
            _constantFlow.updateFlow(
                superToken,
                address(this),
                _userFlows[sender][i].to,
                _userFlows[sender][i].flowRate
            );
        }

        _appFlows[address(this)][sender] += totalOutFlowRate;
        require(totalOutFlowRate <= newFlowRate, "MultiApp: Receiving flow don't cover the costs");
    }

    function beforeAgreementTerminated(
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId
    )
        external
        view
        override
    {
        //noop
    }

    function afterAgreementTerminated(
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata data
    )
    external
    override
    returns (bytes memory newData)
    {
        (address sender, int256 oldFlowRate) = _unpack(ctx);
        delete _userFlows[sender];
        delete _appFlows[address(this)][sender];
        _constantFlow.deleteFlow(superTokenAddr, address(this), sender);
    }

    function _pack(address account, int256 flowRate) internal pure returns(bytes memory) {
        return abi.encodePacked(account, flowRate);
    }

    function _unpack(bytes memory data) internal pure returns(address, int256) {
        return abi.decode(data, (address, int256));
    }
}
