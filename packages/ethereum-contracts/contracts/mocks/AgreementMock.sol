// SPDX-License-Identifier: AGPLv3
pragma solidity 0.7.6;
pragma abicoder v2;

import {
    ISuperfluid,
    ISuperfluidToken,
    ISuperApp,
    SuperAppDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "../agreements/AgreementBase.sol";
import { AgreementLibrary } from "../agreements/AgreementLibrary.sol";


contract AgreementMock is AgreementBase {

    uint256 constant private _REAL_TIME_BALANCE_SLOT_ID = 65552025;

    // using immutable, otherweise the proxy contract would see zero instead
    bytes32 immutable private _type;
    uint immutable private _version;

    constructor(bytes32 t, uint v) {
        _type = t;
        _version = v;
    }

    function version() external view returns (uint) { return _version; }

    /// @dev ISuperAgreement.agreementType implementation
    function agreementType() external override view returns (bytes32) {
        return _type;
    }

    /// @dev ISuperAgreement.realtimeBalanceOf implementation
    function realtimeBalanceOf(
       ISuperfluidToken token,
       address account,
       uint256 /* time */
    )
       external view override
       returns (
           int256 dynamicBalance,
           uint256 deposit,
           uint256 owedDeposit
       )
    {
        bytes32[] memory slotData = token.getAgreementStateSlot(
            address(this), account, _REAL_TIME_BALANCE_SLOT_ID, 3);
        return (
            int256(slotData[0]),
            uint256(slotData[1]),
            uint256(slotData[2])
        );
    }

    /**
     * Real-time balance mockings
     */

    function setRealtimeBalanceFor(
        ISuperfluidToken token,
        address account,
        int256 dynamicBalance,
        uint256 deposit,
        uint256 owedDeposit
    ) external {
        bytes32[] memory slotData = new bytes32[](3);
        slotData[0] = bytes32(uint256(dynamicBalance));
        slotData[1] = bytes32(uint256(deposit));
        slotData[2] = bytes32(uint256(owedDeposit));
        token.updateAgreementStateSlot(account, _REAL_TIME_BALANCE_SLOT_ID, slotData);
    }

    /**
     * Agreement client mockings
     */

    function createAgreementFor(
        ISuperfluidToken token,
        bytes32 id,
        bytes32[] calldata data
        ) external {
        token.createAgreement(id, data);
    }

    function updateAgreementDataFor(
        ISuperfluidToken token,
        bytes32 id,
        bytes32[] calldata data
        ) external {
        token.updateAgreementData(id, data);
    }

    function terminateAgreementFor(
        ISuperfluidToken token,
        bytes32 id,
        uint dataLength
        ) external {
        token.terminateAgreement(id, dataLength);
    }

    function updateAgreementStateSlotFor(
        ISuperfluidToken token,
        address account,
        uint256 slotId,
        bytes32[] calldata slotData
        ) external {
        token.updateAgreementStateSlot(account, slotId, slotData);
    }

    function settleBalanceFor(
        ISuperfluidToken token,
        address account,
        int256 delta
        ) external {
        token.settleBalance(account, delta);
    }

    function makeLiquidationPayoutsFor(
        ISuperfluidToken token,
        bytes32 id,
        address liquidator,
        address penaltyAccount,
        uint256 rewardAmount,
        uint256 bailoutAmount
    ) external {
        token.makeLiquidationPayouts(id, liquidator, penaltyAccount, rewardAmount, bailoutAmount);
    }

    /**
     * Agreement Framework mockings
     */

    function tryCallAppBeforeCallback(ISuperfluid host) external {
        host.callAppBeforeCallback(ISuperApp(address(0)), "", false, "");
    }

    function tryCallAppAfterCallback(ISuperfluid host) external {
        host.callAppAfterCallback(ISuperApp(address(0)), "", false, "");
    }

    function tryAppCallbackPush(ISuperfluid host) external {
        host.appCallbackPush("", ISuperApp(address(0)), 0, 0);
    }

    function tryAppCallbackPop(ISuperfluid host) external {
        host.appCallbackPop("", 0);
    }

    function tryCtxUseAllowance(ISuperfluid host) external {
        host.ctxUseAllowance("", 0, 0);
    }

    function tryJailApp(ISuperfluid host) external {
        host.jailApp("", ISuperApp(address(0)), 0);
    }

    function doRevert(string calldata reason, bytes calldata ctx) external view validCtx(ctx) {
        revert(reason);
    }

    event Pong(uint256 ping);

    function pingMe(address expectedMsgSender, uint256 ping, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        ISuperfluid.Context memory context = ISuperfluid(msg.sender).decodeCtx(ctx);
        assert(context.msgSender == expectedMsgSender);
        emit Pong(ping);
        return ctx;
    }

    event AppBeforeCallbackResult(bytes cbdata);

    function callAppBeforeAgreementCreatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP;
        bytes memory cbdata = AgreementLibrary.callAppBeforeCallback(cbStates, ctx);
        emit AppBeforeCallbackResult(cbdata);
        newCtx = ctx;
    }

    function callAppAfterAgreementCreatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP;
        (, newCtx) = AgreementLibrary.callAppAfterCallback(cbStates, "", ctx);
        require(ISuperfluid(msg.sender).isCtxValid(newCtx), "AgreementMock: ctx not valid after");
    }

    function callAppBeforeAgreementTerminatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP;
        bytes memory cbdata = AgreementLibrary.callAppBeforeCallback(cbStates, ctx);
        emit AppBeforeCallbackResult(cbdata);
        newCtx = ctx;
    }

    function callAppAfterAgreementTerminatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;
        (, newCtx) = AgreementLibrary.callAppAfterCallback(cbStates, "", ctx);
        require(ISuperfluid(msg.sender).isCtxValid(newCtx), "AgreementMock: ctx not valid after");
    }

    modifier validCtx(bytes memory ctx) {
        require(ISuperfluid(msg.sender).isCtxValid(ctx), "AgreementMock: ctx not valid before");
        _;
    }
}
