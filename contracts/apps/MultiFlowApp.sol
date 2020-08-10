// SPDX-License-Identifier: MIT
/* solhint-disable*/
pragma solidity ^0.6.0;

import "../interface/AppHelper.sol";
import "../interface/ISuperApp.sol";
import "../interface/IFlowAgreement.sol";

contract MultiFlowsApp is ISuperApp {

    struct ReceiverData {
        address to;
        int256 flowRate;
    }

    IFlowAgreement internal _constantFlow;
    mapping(address => ReceiverData[]) internal _receiversMap;

    constructor(IFlowAgreement constantFlow) public {
        require(address(constantFlow) != address(0), "SA: can't set zero address as constant Flow");
        _constantFlow = constantFlow;
    }

    function implementationBitmask() external override view returns(uint) {
        return AppHelper.BEFORE_AGREEMENT_CREATED_NOOP |
            AppHelper.AFTER_AGREEMENT_CREATED_NOOP |
            AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP;
    }

    function createMultiFlows(
        ISuperToken superTokenAddr,
        address[] calldata receivers,
        int256[] calldata flowRates
    )
        external
    {
        require(receivers.length == flowRates.length, "SA: number receivers not equal flowRates");
        for(uint256 i = 0; i < receivers.length; i++) {
            _constantFlow.createFlow(ISuperToken(superTokenAddr), msg.sender, receivers[i], flowRates[i]);
            _receiversMap[msg.sender].push(ReceiverData(receivers[i], flowRates[i]));
        }
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
        ISuperToken superTokenAddr,
        bytes calldata ctx,
        address agreementClass,
        bytes32 agreementId,
        bytes calldata data
    )
    external
    override
    {
        (address sender, int256 oldFlowRate) = _unpack(data);
        (, , , int256 newFlowRate) = _constantFlow.getFlow(superTokenAddr, agreementId);
        int256 scaleFactor = newFlowRate / oldFlowRate;
        uint256 size = _receiversMap[msg.sender].length;
        for(uint256 i = 0; i < size; i++) {
            _receiversMap[msg.sender][i].flowRate *= scaleFactor;
            _constantFlow.updateFlow(
                superTokenAddr,
                msg.sender,
                _receiversMap[msg.sender][i].to,
                _receiversMap[msg.sender][i].flowRate
            );
        }
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
        (, , , int256 flowRate) = _constantFlow.getFlow(superTokenAddr, agreementId);
        uint256 size = _receiversMap[msg.sender].length;
        int256 scaleFactor = flowRate / oldFlowRate;
        for(uint256 i = 0; i < size; i++) {
            _receiversMap[msg.sender][i].flowRate *= scaleFactor;
            _constantFlow.deleteFlow(superTokenAddr, msg.sender, _receiversMap[msg.sender][i].to);
        }
    }

    function _pack(address account, int256 flowRate) internal pure returns(bytes memory) {
        return abi.encodePacked(account, flowRate);
    }

    function _unpack(bytes memory data) internal pure returns(address, int256) {
        return abi.decode(data, (address, int256));
    }
}
