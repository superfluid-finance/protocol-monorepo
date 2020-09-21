// SPDX-License-Identifier: MIT
/* solhint-disable not-rely-on-time */
pragma solidity 0.7.0;

import { IConstantFlowAgreementV1, ISuperToken } from "../interfaces/IConstantFlowAgreementV1.sol";
import { ISuperfluidGovernance } from "../interfaces/ISuperfluidGovernance.sol";
import { ISuperfluid } from "../interfaces/ISuperfluid.sol";
import { Math } from "@openzeppelin/contracts/math/Math.sol";
import { SignedSafeMath } from "@openzeppelin/contracts/math/SignedSafeMath.sol";
import { SafeMath } from "@openzeppelin/contracts/math/SafeMath.sol";
import { AgreementLibrary } from "./AgreementLibrary.sol";
import { ContextLibrary } from "../superfluid/ContextLibrary.sol";

contract ConstantFlowAgreementV1 is IConstantFlowAgreementV1 {

    event DebugAccount(string msg, address account);
    event DebugFlowRate(string msg, int96 flowRate);
    event DebugDeposit(uint256 deposit);
    event DebugWord(string msg, bytes32 word);

    using SignedSafeMath for int256;
    using SafeMath for uint256;
    using ContextLibrary for ContextLibrary.Context;

    struct AgreementData {
        uint256 timestamp;
        address sender;
        address receiver;
        int96 flowRate;
        uint256 depositFactor;
        uint256 owedDepositFactor;
        uint256 deposit;
        uint256 owedDeposit;
    }

    struct AccountState {
        uint256 timestamp;
        int96 flowRate;
        uint256 deposit;
        uint256 owedDeposit;
    }

    /*
    normalizeDeposit(flowRate, factor) gives the deposit number to be used in:
    - depositAllowanceUsed
    - account state

    - truncate last 9 decimals
    */

    /*
     * ISuperAgreement interface
     */
    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
        ISuperToken token,
        address account,
        bytes calldata /*data*/,
        uint256 time
    )
        external
        view
        override
        returns (int256 dynamicBalance, uint256 deposit, uint256 owedDeposit)
    {
        (bool exist, AccountState memory state) = _getAccountState(token, account);
        if(exist) {
            dynamicBalance = ((int256(time).sub(int256(state.timestamp))).mul(state.flowRate));
            deposit = state.deposit;
            owedDeposit = state.owedDeposit;
        }
    }

    /// @dev ISuperAgreement.touch implementation
    function touch(
        address /* account */,
        bytes memory /*currentData*/,
        uint256 /* timestamp */
    )
        public
        pure
        override
        returns(bytes memory /*newData*/)
    {
        //(, int256 cRate, int256 cDeposit, int256 cOwned) = _decodeFlow(currentData);
        //return _encodeFlow(timestamp, cRate, cDeposit, cOwned);
        int a = 1;
        a++;
    }

    /// @dev IFlowAgreement.createFlow implementation
    function createFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO: Decode return cbdata before calling the next step
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);
        bytes32 flowId = _generateId(stcCtx.msgSender, receiver);
        require(_isNewFlow(token, flowId), "Flow already exist");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementCreated(
                ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );

        (uint256 depositSpend, , AgreementData memory newData) =
            _updateFlow(token, stcCtx.msgSender, receiver, flowRate, 0, 0);
        newCtx = ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, depositSpend);

        newCtx = AgreementLibrary.afterAgreementCreated(
            ISuperfluid(msg.sender),
            token,
            newCtx,
            address(this),
            receiver,
            flowId,
            cbdata
        );

        ContextLibrary.Context memory stcNewCtx = ContextLibrary.decode(newCtx);
        if(stcCtx.allowance == 0 && stcCtx.allowanceUsed == 0) {
            newData.deposit = stcNewCtx.allowanceUsed;
            newData.owedDeposit = 0;
        } else {
            newData.deposit = depositSpend;
            newData.owedDeposit = (stcCtx.allowance > depositSpend
                ? depositSpend : depositSpend - stcCtx.allowance);
        }

        _updateDeposits(token, newData, stcNewCtx.msgSender, flowId, 0, 0);
        (newCtx, ) = ContextLibrary.encode(stcNewCtx);
        _isNewFlow(token, flowId);
    }

    function updateFlow(
        ISuperToken token,
        address receiver,
        int96 flowRate,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO meta-tx support
        // TODO: Decode return cbdata before calling the next step
        ContextLibrary.Context memory stcCtx = ContextLibrary.decode(ctx);
        bytes32 flowId = _generateId(stcCtx.msgSender, receiver);
        require(!_isNewFlow(token, flowId), "Flow doesn't exist");
        //require(sender == msg.sender, "FlowAgreement: only sender can update its own flow");
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementUpdated(
            ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );

        (uint256 depositSpend, , AgreementData memory newData) =
            _updateFlow(token, stcCtx.msgSender, receiver, flowRate, 0, 0);
        newCtx = ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, depositSpend);

        newCtx = AgreementLibrary.afterAgreementUpdated(
            ISuperfluid(msg.sender),
            token,
            ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, depositSpend),
            address(this),
            receiver,
            flowId,
            cbdata
        );

        ContextLibrary.Context memory stcNewCtx = ContextLibrary.decode(newCtx);
        if(stcCtx.allowance == 0 && stcCtx.allowanceUsed == 0) {
            newData.deposit = stcNewCtx.allowanceUsed;
            newData.owedDeposit = 0;
        } else {
            newData.deposit = depositSpend;
            newData.owedDeposit = (stcCtx.allowance > depositSpend
                ? depositSpend : depositSpend - stcCtx.allowance);
        }

        _updateDeposits(token, newData, stcNewCtx.msgSender, flowId, 0, 0);
        (newCtx, ) = ContextLibrary.encode(stcNewCtx);
    }

    /// @dev IFlowAgreement.deleteFlow implementation
    function deleteFlow(
        ISuperToken token,
        address sender,
        address receiver,
        bytes calldata ctx
    )
        external
        override
        returns(bytes memory newCtx)
    {
        // TODO: Decode return cbdata before calling the next step
        address msgSender = ContextLibrary.decode(ctx).msgSender;
        bytes32 flowId = _generateId(sender, receiver);
        bool isLiquidator = (msgSender != sender && msgSender != receiver);
        if (isLiquidator) {
            require(token.isAccountInsolvent(sender),
                    "FlowAgreement: account is solvent");
        }
        bytes memory cbdata;
        (cbdata, newCtx) =
            AgreementLibrary.beforeAgreementTerminated(
                ISuperfluid(msg.sender), token, ctx, address(this), receiver, flowId
        );
        _terminateAgreementData(token, msgSender, sender, receiver, isLiquidator);
        newCtx = AgreementLibrary.afterAgreementTerminated(
            ISuperfluid(msg.sender),
            token,
            ContextLibrary.updateCtxDeposit(ISuperfluid(msg.sender), receiver, newCtx, 0),
            address(this),
            receiver,
            flowId,
            cbdata
        );
    }

    /// @dev IFlowAgreement.getFlow implementation
    function getFlow(
        ISuperToken token,
        address sender,
        address receiver
    )
        external
        view
        override
        returns(int96 flowRate)
    {
        (, AgreementData memory data) = _getAgreementData(
            token,
            keccak256(abi.encode(sender, receiver))
        );
        return data.flowRate;
    }

    /// @dev IFlowAgreement.getFlow implementation
    function getFlow(
        ISuperToken token,
        bytes32 flowId
    )
        external
        view
        override
        returns(
            uint256 timestamp,
            address sender,
            address receiver,
            int96 flowRate,
            uint256 deposit,
            uint256 owedDeposit
        )
    {
        (, AgreementData memory data) = _getAgreementData(
            token,
            flowId
        );
        return(
            data.timestamp,
            data.sender,
            data.receiver,
            data.flowRate,
            data.deposit,
            data.owedDeposit
        );
    }

    /// @dev IFlowAgreement.getNetFlow implementation
    function getNetFlow(
        ISuperToken token,
        address account
    )
        external
        view
        override
        returns (int96 flowRate)
    {
        (, AccountState memory state) = _getAccountState(token, account);
        return state.flowRate;
    }

    /*
     * Internal Functions
     */

    function _updateAccountState(
        ISuperToken token,
        address account,
        int96 flowRate,
        uint256 normalizeDeposit,
        uint256 normalizeOwedDeposit
    )
        private
        returns(int96 newFlowRate)
    {
        (, AccountState memory state) = _getAccountState(token, account);
        state.flowRate += flowRate;
        state.deposit += normalizeDeposit;
        state.owedDeposit += normalizeOwedDeposit;
        token.updateAgreementStateSlot(account, 0, _encodeAccountState(state));
        return state.flowRate;
    }

    function _updateFlow(
        ISuperToken token,
        address sender,
        address receiver,
        int96 flowRate,
        uint256 deposit,
        uint256 owedDeposit
    )
        private
        returns(uint256 allowanceUsed, uint256 /*oldDeposit*/, AgreementData memory newData)
    {
        require(sender != receiver, "FlowAgreement: self flow not allowed");
        require(flowRate != 0, "FlowAgreement: use delete flow");
        require(flowRate > 0, "FlowAgreement: negative flow rate not allowed");

        bytes32 flowId = _generateId(sender, receiver);
        (, AgreementData memory data) = _getAgreementData(token, flowId);
        allowanceUsed = _minimalDeposit(token, flowRate);
        newData = AgreementData(
            block.timestamp,
            sender,
            receiver,
            flowRate,
            0,
            0,
            deposit,
            owedDeposit
        );
        token.createAgreement2(flowId, _encodeAgreementData(newData));
        int96 totalSenderFlowRate = _updateAccountState(
            token,
            sender,
            -(flowRate - data.flowRate),
            0,
            0
        );
        int96 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            flowRate - data.flowRate,
            0,
            0
        );
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
        address caller,
        address sender,
        address receiver,
        bool liquidation
    )
        private
        returns(uint256 deposit, uint256 owedDeposit)
    {
        bytes32 flowId = _generateId(sender, receiver);
        (bool exist, AgreementData memory data) = _getAgreementData(token, flowId);
        require(exist, "FlowAgreement: flow does not exist");

        int256 totalSenderFlowRate = _updateAccountState(
            token,
            sender,
            data.flowRate,
            -data.deposit,
            -data.owedDeposit
        );
        int256 totalReceiverFlowRate = _updateAccountState(
            token,
            receiver,
            -data.flowRate,
            0,
            0
        );

        // Close this Agreement Data
        if (liquidation) {
            token.liquidateAgreement(
                caller,
                flowId,
                sender,
                deposit
            );
        } else {
            token.terminateAgreement2(flowId, 2);
        }

        emit FlowUpdated(
            token,
            sender,
            receiver,
            0,
            totalSenderFlowRate,
            totalReceiverFlowRate);
    }

    function _generateId(address sender, address receiver) private pure returns(bytes32 id) {
        require(sender != address(0), "Sender is zero");
        require(receiver != address(0), "Receiver is zero");

        return keccak256(abi.encodePacked(sender, receiver));
    }

    function _isNewFlow(
        ISuperToken token,
        bytes32 flowId
    )
        internal
        //view
        returns(bool isNewFlow)
    {
        (bool exist, ) = _getAgreementData(token, flowId);
        return !exist;
    }

    function _minimalDeposit(ISuperToken token, int96 flowRate) internal view returns(uint256 deposit) {
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernance());
        uint16 liquidationPeriod = gov.getLiquidationPeriod(token.getUnderlayingToken());
        deposit = _normalizeDeposit(flowRate, liquidationPeriod);
    }

    function _getDepositFactor(ISuperToken token) internal view returns(uint16 factor) {
        ISuperfluidGovernance gov = ISuperfluidGovernance(token.getGovernance());
        factor = gov.getLiquidationPeriod(token.getUnderlayingToken());
    }

    function _updateDeposits(
        ISuperToken token,
        AgreementData memory data,
        address account,
        bytes32 flowId,
        uint256 deposit,
        uint256 owedDeposit
    )
        internal
    {
        //update data deposit and save it
        data.depositFactor = uint256(_getDepositFactor(token));
        data.owedDepositFactor = owedDeposit / uint256(data.flowRate);
        token.updateAgreementData2(flowId, _encodeAgreementData(data));
        _updateAccountState(
            token,
            account,
            data.flowRate,
            (deposit / 1e9 * 1e9),
            (owedDeposit / 1e9 * 1e9)
        );
    }

    function _encodeAccountState
    (
        AccountState memory astate
    )
        private
        pure
        returns(bytes32[] memory state)
    {
        state = new bytes32[](1);
        state[0] = bytes32(
            (uint256(astate.timestamp)) << 224 |
            (uint256(astate.flowRate)) << 128 |
            (uint256(astate.deposit)) <<  64 |
            (uint256(astate.owedDeposit))
        );
    }

    function _encodeAgreementData
    (
        AgreementData memory adata
    )
        private
        pure
        returns (bytes32[] memory data)
    {
        data = new bytes32[](2);

        data[0] = bytes32(
            (uint256(adata.sender) << 96) |
            (uint256(adata.flowRate))
        );

        data[1] = bytes32(
            (uint256(adata.receiver) << (12*8)) |
            (uint256(adata.depositFactor) << 64) |
            (uint256(adata.owedDepositFactor) << 32) |
            uint256(adata.timestamp)
        );
    }

    function _getAccountState
    (
        ISuperToken token,
        address account
    )
        private
        view
        returns(bool exist, AccountState memory state)
    {
        bytes32[] memory data = token.getAgreementStateSlot(address(this), account, 0, 1);
        uint256 a = uint256(data[0]);
        exist = a > 0;
        if (exist) {
            state.timestamp = uint32(a >> 224);
            state.flowRate = int96((a >> 128) & uint96(int96(-1)));
            state.deposit = uint64(a >> 64 & uint64(-1));
            state.owedDeposit = uint64(a & uint64(-1));
        }
    }

    function _getAgreementData
    (
        ISuperToken token,
        bytes32 dId
    )
        private
        view
        returns (bool exist, AgreementData memory adata)
    {
        bytes32[] memory data = token.getAgreementData2(address(this), dId, 2);
        uint256 a = uint256(data[0]);
        uint256 b = uint256(data[1]);
        exist = a > 0;
        if (exist) {
            adata.sender = address(uint160(a >> (12*8)));
            adata.flowRate = int96(a & uint96(int96(-1)));

            adata.receiver = address(uint160(b >> (12*8)));
            adata.depositFactor = uint32(b & uint32(int32(-1)));
            adata.owedDepositFactor = uint32(b >> 32);
            adata.timestamp = uint32(b >> 32);
            adata.deposit = _normalizeDeposit(
                int96((a >> 96) & uint96(int96(-1))),
                uint32(b & uint32(int32(-1)))
            );
            adata.owedDepositFactor = _normalizeOweDeposit(uint32(b >> 32));

        }
    }

    //FIXME add 1 so the deposit is not zero
    function _normalizeDeposit(
        int96 flowRate,
        uint32 factor
    )
        internal
        pure
        returns(uint256 deposit)
    {
        deposit = uint256((flowRate * int96(factor)) / 1e9 * 1e9);
    }

    function _normalizeOweDeposit(
        uint32 owedDeposit_
    )
        internal
        pure
        returns(uint256 owedDeposit)
    {
        owedDeposit = owedDeposit_ / 1e9 * 1e9;
    }
}
