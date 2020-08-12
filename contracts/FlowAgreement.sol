// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.6.6;

import { ISuperToken, IFlowAgreement } from "./interface/IFlowAgreement.sol";
import { ISuperfluidGovernance } from "./interface/ISuperfluidGovernance.sol";
import { ISuperfluid } from "./interface/ISuperfluid.sol";
import { ISuperApp } from "./interface/ISuperApp.sol";
import { AppHelper } from "./interface/AppHelper.sol";
import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";

contract FlowAgreement is IFlowAgreement {

    using SignedSafeMath for int256;

    function agreementType() external override pure returns (bytes32) {
        return keccak256("org.superfluid-finance.agreements.ConstantFlowAgreement.v1");
    }

    /*
     * ISuperAgreement interface
     */
    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        bytes calldata data,
        uint256 time
    )
        external
        pure
        override
        returns (int256 balance)
    {
        uint256 startDate;
        int256 flowRate;

        (startDate, flowRate) = _decodeFlow(data);
        return (int256(time).sub(int256(startDate))).mul(flowRate);
    }

    /// @dev ISuperAgreement.touch implementation
    function touch(
        bytes memory currentData,
        uint256 timestamp
    )
        public
        pure
        override
        returns(bytes memory newData)
    {
        (, int256 cRate) = _decodeFlow(currentData);
        return _encodeFlow(timestamp, cRate);
    }

    /// @dev IFlowAgreement.createFlow implementation
    function createFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external
        override
    {
        bytes32 flowId = _generateId(sender, receiver);
        require(_isNewFlow(token, flowId), "Flow already exist");
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
        uint64 gasReserved = gov.getGasReservation();
        // TODO: THIS CAN REVERT AND STOP EVERYTHING
        bytes memory data = abi.encode(address(this), flowId, flowRate);
        bytes memory ctx;
        (bool ok, uint bitmask) = _checkCaller(gov, sender);
        if(ok && (bitmask & AppHelper.BEFORE_AGREEMENT_CREATED_NOOP) != AppHelper.BEFORE_AGREEMENT_CREATED_NOOP) {
            ctx = ISuperfluid(gov.getSuperfluid()).callBuildContext(
                msg.sender,
                gasReserved,
                ISuperApp(msg.sender).beforeAgreementCreated.selector,
                data
            );
        }
        _updateFlow(token, sender, receiver, flowRate);
        if(ok && (AppHelper.AFTER_AGREEMENT_CREATED_NOOP & bitmask) != AppHelper.AFTER_AGREEMENT_CREATED_NOOP) {
            ISuperfluid(gov.getSuperfluid()).callWithContext(
                ctx,
                msg.sender,
                ISuperApp(msg.sender).afterAgreementCreated.selector,
                data
            );
        }
    }

    /*
     *   IFlowAgreement interface
     */
    /// @dev IFlowAgreement.updateFlow implementation
    function updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        external
        override
    {
        // TODO meta-tx support
        bytes32 flowId = _generateId(sender, receiver);
        require(!_isNewFlow(token, flowId), "Flow doesn't exist");
        require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
        uint64 gasReserved = gov.getGasReservation();
        // TODO: THIS CAN REVERT AND STOP EVERYTHING
        bytes memory data = abi.encode(address(this), flowId, flowRate);
        bytes memory ctx;
        (bool ok, uint bitmask) = _checkCaller(gov, msg.sender);
        if(ok && (AppHelper.BEFORE_AGREEMENT_UPDATED_NOOP & bitmask) != AppHelper.BEFORE_AGREEMENT_UPDATED_NOOP) {
            ctx = ISuperfluid(gov.getSuperfluid()).callBuildContext(
                msg.sender,
                gasReserved,
                ISuperApp(msg.sender).beforeAgreementCreated.selector,
                data
            );
        }
        _updateFlow(token, sender, receiver, flowRate);
        if(ok && AppHelper.AFTER_AGREEMENT_UPDATED_NOOP & bitmask != AppHelper.AFTER_AGREEMENT_UPDATED_NOOP) {
            ISuperfluid(gov.getSuperfluid()).callWithContext(
                ctx,
                msg.sender,
                ISuperApp(msg.sender).afterAgreementCreated.selector,
                data
            );
        }
    }

    /// @dev IFlowAgreement.getFlow implementation
    function getFlow(
        ISuperToken token,
        bytes32 flowId
    )
        external
        view
        override
        returns (uint256 timestamp, address sender, address receiver, int256 flowRate)
    {
        bytes memory data = token.getAgreementData(address(this), flowId);
        return _decodeData(data);
    }

    /// @dev IFlowAgreement.getNetFlow implementation
    function getNetFlow(
        ISuperToken token,
        address account
    )
        external
        view
        override
        returns (int256 flowRate)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        (, flowRate) = _decodeFlow(state);
    }

    /// @dev IFlowAgreement.deleteFlow implementation
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        override
    {
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
        uint64 gasReserved = gov.getGasReservation();
        // TODO: THIS CAN REVERT AND STOP EVERYTHING
        bytes memory data = abi.encode(address(this), _generateId(sender, receiver));
        bytes memory ctx;
        (bool ok, uint bitmask) = _checkCaller(gov, msg.sender);
        if(ok && (AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP & bitmask) != AppHelper.BEFORE_AGREEMENT_TERMINATED_NOOP) {
            ctx = ISuperfluid(gov.getSuperfluid()).callBuildContext(
                msg.sender,
                gasReserved,
                ISuperApp(msg.sender).beforeAgreementCreated.selector,
                data
            );
        }

        bool isLiquidator = (msg.sender != sender && msg.sender != receiver);
        if (isLiquidator) {
            require(token.isAccountInsolvent(sender), "FlowAgreement: account is solvent");
        }

        _terminateAgreementData(token, sender, receiver, isLiquidator);
    }

    /*
     * Internal Functions
     */

    function _updateAccountState(
        ISuperToken token,
        address account,
        int256 flowRate
    )
        private
        returns(int256 newFlowRate)
    {
        bytes memory state = token.getAgreementAccountState(address(this), account);
        state = _composeState(state, flowRate, block.timestamp);
        token.updateAgreementAccountState(account, state);
        (, newFlowRate) = _decodeFlow(state);
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
    {
        require(sender != receiver, "FlowAgreement: self flow not allowed");
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory oldFlowData = token.getAgreementData(address(this), flowId);
        (, , , int256 oldFlowRate) = _decodeData(oldFlowData);
        bytes memory newFlowData = _encodeData(block.timestamp, sender, receiver, flowRate);
        token.createAgreement(flowId, newFlowData);

        //(, int256 newFlowRate) = _decodeFlow(newFlowData);
        int flowRateDelta = flowRate - oldFlowRate;

        int256 totalSenderFlowRate = _updateAccountState(token, sender, _mirrorFlowRate(flowRateDelta));
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, flowRateDelta);
        emit FlowUpdated(
            token,
            sender,
            receiver,
            flowRate,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _terminateAgreementData(
        ISuperToken token,
        address sender,
        address receiver,
        bool liquidation
    )
        private
    {
        bytes32 flowId = _generateId(sender, receiver);
        bytes memory flowData = token.getAgreementData(address(this), flowId);
        require(flowData.length > 0, "FlowAgreement: flow does not exist");
        (, , , int256 senderFlowRate) = _decodeData(flowData);
        require(senderFlowRate > 0, "FlowAgreement: sender flow rate must be positive");

        int256 totalSenderFlowRate = _updateAccountState(token, sender, senderFlowRate);
        int256 totalReceiverFlowRate = _updateAccountState(token, receiver, _mirrorFlowRate(senderFlowRate));

        // note : calculate the deposit here and pass it to superToken, and emit the events.
        // Close this Agreement Data
        if (liquidation) {
            ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernanceAddress());
            uint16 minDeposit = gov.getMinimalDeposit(token.getUnderlayingToken());
            uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
            uint256 deposit = Math.max(
                uint256(minDeposit),
                uint256(senderFlowRate) * uint256(liquidationPeriod));

            token.liquidateAgreement(msg.sender, flowId, sender, deposit);
        } else {
            token.terminateAgreement(flowId);
        }

        emit FlowUpdated(
            token,
            sender,
            receiver,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _mirrorFlowRate(int256 flowRate) private pure returns(int256 mirrorFlowRate) {
        return -1 * flowRate;
    }

    function _generateId(address sender, address receiver) private pure returns(bytes32) {
        require(sender != address(0), "Sender is zero");
        require(receiver != address(0), "Receiver is zero");

        return keccak256(abi.encodePacked(sender, receiver));
    }

    function _checkCaller(ISuperfluidGovernance gov, address caller) private view returns(bool, uint) {
        ISuperfluid superfluid = ISuperfluid(gov.getSuperfluid());

        if(superfluid.isWhiteListed(caller, msg.sender)) {
            uint256 bitmask = superfluid.getConfig(msg.sender);
            return (bitmask > 0, bitmask);
        }

        return (false, 0);
    }

    function _encodeData
    (
        uint256 timestamp,
        address sender,
        address receiver,
        int256 flowRate
    )
        private
        pure
        returns(bytes memory)
    {
        return abi.encode(timestamp, sender, receiver, flowRate);
    }

    function _decodeData
    (
        bytes memory data
    )
        private
        pure
        returns
    (
        uint256 timestamp,
        address sender,
        address receiver,
        int256 flowRate
    )
    {
        if (data.length == 0) return (0, address(0), address(0), 0);
        //require(data.length == 104, "FlowAgreement: invalid data");
        return abi.decode(data, (uint256, address, address, int256));
    }


    /// Encoders & Decoders
    /// @dev Encode the parameters into a bytes type.
    ///      Both data and state share the same data structure.
    function _encodeFlow
    (
        uint256 timestamp,
        int256 flowRate
    )
        private
        pure
        returns (bytes memory)
    {
        return abi.encode(timestamp, flowRate);
    }

    /// @dev Decode the parameter into the original types
    function _decodeFlow
    (
        bytes memory state
    )
        private
        pure
        returns
    (
        uint256 timestamp,
        int256 flowRate
    )
    {
        if (state.length == 0) return (0, 0);
        require(state.length == 64, "FlowAgreement: invalid state");
        return abi.decode(state, (uint256, int256));
    }

    /// @notice Compose in one state the states passed as arguments.
    /// @dev Will add the two state and update the `timestamp` to block.timestamp
    /// @dev If end result is a FlowRate of zero then return String.Empty
    /// @param currentState Data of the actual agreement
    /// @param flowRate New value to update
    /// @param timestamp New time to update
    /// @return newAgreement New agreement data
    function _composeState
    (
        bytes memory currentState,
        int256 flowRate,
        uint256 timestamp
    )
        private
        pure
        returns (bytes memory newAgreement)
    {
        (, int cRate) = _decodeFlow(currentState);
        cRate = cRate.add(flowRate);

        if (cRate == 0) {
            return "";
        }
        return _encodeFlow(timestamp, cRate);
    }

    function _isNewFlow(
        ISuperToken token,
        bytes32 flowId
    )
        internal
        view
        returns(bool)
    {
        bytes memory data = token.getAgreementData(address(this), flowId);
        return (data.length == 0);
    }
}
