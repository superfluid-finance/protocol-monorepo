// SPDX-License-Identifier: AGPLv3
pragma solidity 0.8.12;

import {
    ISuperfluid,
    ISuperfluidToken,
    ISuperApp,
    SuperAppDefinitions
} from "../interfaces/superfluid/ISuperfluid.sol";
import { AgreementBase } from "../agreements/AgreementBase.sol";
import { AgreementLibrary } from "../agreements/AgreementLibrary.sol";
import { SafeCast } from "@openzeppelin/contracts/utils/math/SafeCast.sol";

contract AgreementMock is AgreementBase {

    using SafeCast for uint256;

    uint256 constant private _REAL_TIME_BALANCE_SLOT_ID = 65552025;

    // using immutable, otherweise the proxy contract would see zero instead
    bytes32 immutable private _type;
    uint immutable private _version;

    constructor(address host, bytes32 t, uint v) AgreementBase(host) {
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
            uint256(slotData[0]).toInt256(),
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
        bool useDefaultRewardAccount,
        address liquidator,
        address targetAccount,
        uint256 rewardAmount,
        int256 targetAccountBalanceDelta
    ) external {
        bytes memory liquidationTypeData = useDefaultRewardAccount
            ? abi.encode(1, 0)
            : abi.encode(1, 2);
        token.makeLiquidationPayoutsV2(
            id,
            liquidationTypeData,
            liquidator,
            useDefaultRewardAccount,
            targetAccount,
            rewardAmount,
            targetAccountBalanceDelta
        );
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
        host.appCallbackPush("", ISuperApp(address(0)), 0, 0, ISuperfluidToken(address(0)));
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

    event AppBeforeCallbackResult(
        uint8 appLevel,
        uint8 callType,
        bytes4 agreementSelector,
        bytes cbdata);

    event AppAfterCallbackResult(
        uint8 appLevel,
        uint8 callType,
        bytes4 agreementSelector);

    function _callAppBeforeCallback(
        ISuperApp app,
        uint256 noopBit,
        bytes calldata ctx
    )
        private
    {
        ISuperfluid.Context memory context = ISuperfluid(msg.sender).decodeCtx(ctx);
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = noopBit;
        bytes memory cbdata = AgreementLibrary.callAppBeforeCallback(cbStates, ctx);
        emit AppBeforeCallbackResult(
            context.appLevel,
            context.callType,
            context.agreementSelector,
            cbdata);
    }

    function _callAppAfterAgreementCallback(
        ISuperApp app,
        uint256 noopBit,
        bytes calldata ctx
    )
        private returns (bytes memory newCtx)
    {
        bool isJailed = ISuperfluid(msg.sender).isAppJailed(app);
        ISuperfluid.Context memory context = ISuperfluid(msg.sender).decodeCtx(ctx);
        AgreementLibrary.CallbackInputs memory cbStates = AgreementLibrary.createCallbackInputs(
            ISuperfluidToken(address(0)) /* token */,
            address(app) /* account */,
            0 /* agreementId */,
            "" /* agreementData */
        );
        cbStates.noopBit = noopBit;
        (, newCtx) = AgreementLibrary.callAppAfterCallback(cbStates, "", ctx);
        if (isJailed) {
            require(newCtx.length == 0, "AgreementMock: callback should not reach jailed app");
            newCtx = ctx;
        } else {
            require(ISuperfluid(msg.sender).isCtxValid(newCtx), "AgreementMock: ctx not valid after callback");
        }
        emit AppAfterCallbackResult(
            context.appLevel,
            context.callType,
            context.agreementSelector);
    }

    function callAppBeforeAgreementCreatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        _callAppBeforeCallback(app, SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP, ctx);
        return ctx;
    }

    function callAppAfterAgreementCreatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        return _callAppAfterAgreementCallback(app, SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP, ctx);
    }

    function callAppBeforeAgreementUpdatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        _callAppBeforeCallback(app, SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP, ctx);
        return ctx;
    }

    function callAppAfterAgreementUpdatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        return _callAppAfterAgreementCallback(app, SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP, ctx);
    }

    function callAppBeforeAgreementTerminatedCallback(
        ISuperApp app,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        _callAppBeforeCallback(app, SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP, ctx);
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
        return _callAppAfterAgreementCallback(app, SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP, ctx);
    }

    modifier validCtx(bytes memory ctx) {
        require(ISuperfluid(msg.sender).isCtxValid(ctx), "AgreementMock: ctx not valid before");
        _;
    }
}
