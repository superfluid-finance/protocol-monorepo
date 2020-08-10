// SPDX-License-Identifier: MIT
/* solhint-disable*/
pragma solidity >=0.6.0;

import "../interface/AppHelper.sol";
import "../interface/ISuperApp.sol";
import "../interface/IFlowAgreement.sol";

contract CashBack is ISuperApp {

    IFlowAgreement internal _constantFlow;
    ISuperToken internal _cashBackSuperToken;

    constructor(IFlowAgreement constantFlow, ISuperToken cashBackSuperToken) public {
        require(address(constantFlow) != address(0), "SA: can't set zero address as constant Flow");
        _constantFlow = constantFlow;
        _cashBackSuperToken = cashBackSuperToken;
    }

    function implementationBitmask() external override view returns(uint) {
        return AppHelper.BEFORE_AGREEMENT_CREATED_NOOP |
            AppHelper.AFTER_AGREEMENT_CREATED_NOOP |
            AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP;
    }

    function startPayment(
        address superTokenAddr,
        address to,
        int256 flowRate
    )
        external
    {
        //Start a new FlowAgreement from msg.sender -> to
        _constantFlow.createFlow(
            ISuperToken(superTokenAddr),
            msg.sender,
            to,
            flowRate
        );

        //Start a new FlowAgreement from address(this) -> msg.sender
        _constantFlow.createFlow(
            _cashBackSuperToken,
            address(this),
            msg.sender,
            flowRate
        );
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
    }

    function _pack(address account, int256 flowRate) internal pure returns(bytes memory) {
        return abi.encodePacked(account, flowRate);
    }

    function _unpack(bytes memory data) internal pure returns(address, int256) {
        return abi.decode(data, (address, int256));
    }
}
