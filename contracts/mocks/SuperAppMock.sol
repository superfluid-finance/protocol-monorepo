// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluid,
    ISuperToken,
    ISuperApp,
    ISuperAgreement
} from "../superfluid/Superfluid.sol";
import { AgreementMock } from "./AgreementMock.sol";


contract SuperAppMock is ISuperApp {

    ISuperfluid private _host;

    constructor(ISuperfluid host, uint256 configWord, bool doubleRegistration) {
        _host = host;
        _host.registerApp(configWord);
        if (doubleRegistration) {
            _host.registerApp(configWord);
        }
    }

    function tryRegisterApp(uint256 configWord) external {
        _host.registerApp(configWord);
    }

    function allowCompositeApp(ISuperApp target) external {
        _host.allowCompositeApp(target);
    }

    /*************************************************************************
    * Test App Actions
    **************************************************************************/

    event NoopEvent();

    function actionNoop(bytes calldata ctx) external validCtx(ctx) returns (bytes memory newCtx) {
        emit NoopEvent();
        return ctx;
    }

    function actionAssert(bytes calldata ctx) external view validCtx(ctx) {
        assert(false);
    }

    function actionRevert(bytes calldata ctx) external view validCtx(ctx) {
        // solhint-disable-next-line reason-string
        revert();
    }

    function actionRevertWithReason(string calldata reason, bytes calldata ctx) external view validCtx(ctx) {
        revert(reason);
    }

    function actionCallAgreementWithoutCtx(bytes calldata ctx) external validCtx(ctx) {
        // this should fail, action should call agreement with ctx
        _host.callAgreement(ISuperAgreement(address(0)), new bytes(0), new bytes(0));
    }

    function actionCallAppActionWithoutCtx(bytes calldata ctx) external validCtx(ctx) {
        // this should fail, action should call agreement with ctx
        _host.callAppAction(ISuperApp(address(0)), new bytes(0));
    }

    function actionAlteringCtx(bytes calldata ctx)
        external view
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        return abi.encode(42);
    }

    function actionPingAgreement(AgreementMock agreement, uint256 ping, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        (newCtx, ) = _host.callAgreementWithContext(
            agreement,
            abi.encodeWithSelector(
                AgreementMock.pingMe.selector,
                ping,
                new bytes(0)
            ),
            new bytes(0), // user data
            ctx);
    }

    function actionAgreementRevert(AgreementMock agreement, string calldata reason, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        (newCtx, ) = _host.callAgreementWithContext(
            agreement,
            abi.encodeWithSelector(
                AgreementMock.doRevert.selector,
                reason,
                new bytes(0)
            ),
            new bytes(0), // user data
            ctx);
    }

    function actionCallActionNoop(bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        newCtx = _host.callAppActionWithContext(
            this,
            abi.encodeWithSelector(
                SuperAppMock.actionNoop.selector,
                new bytes(0)
            ),
            ctx);
    }

    function actionCallActionRevert(string calldata reason, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        newCtx = _host.callAppActionWithContext(
            this,
            abi.encodeWithSelector(
                SuperAppMock.actionRevertWithReason.selector,
                reason,
                new bytes(0)
            ),
            ctx);
    }

    function actionCallAgreementWithInvalidCtx(AgreementMock agreement, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        (newCtx, ) = _host.callAgreementWithContext(
            agreement,
            abi.encodeWithSelector(
                AgreementMock.pingMe.selector,
                42,
                new bytes(0)
            ),
            new bytes(0), // user data
            abi.encode(42));
    }

    function actionCallActionWithInvalidCtx(string calldata reason, bytes calldata ctx)
        external
        validCtx(ctx)
        returns (bytes memory newCtx)
    {
        newCtx = _host.callAppActionWithContext(
            this,
            abi.encodeWithSelector(
                SuperAppMock.actionRevertWithReason.selector,
                reason,
                new bytes(0)
            ),
            abi.encode(42));
    }

    function actionCallBadAction(bytes calldata ctx)
        external
        validCtx(ctx)
    {
        _host.callAppActionWithContext(
            this,
            abi.encodeWithSelector(
                SuperAppMock.actionAlteringCtx.selector,
                new bytes(0)
            ),
            ctx);
        assert(false);
    }

    /*************************************************************************
    * Callbacks
    **************************************************************************/

    enum NextCallbackActionType {
        Noop, // 0
        Assert, // 1
        Revert, // 2
        RevertWithReason, // 3
        AlteringCtx, // 4
        BurnGas // 5
    }

    struct NextCallbackAction {
        NextCallbackActionType actionType;
        bytes data;
    }

    NextCallbackAction private _nextCallbackAction;

    function setNextCallbackAction(
        NextCallbackActionType actionType,
        bytes calldata data)
        external
    {
        _nextCallbackAction.actionType = actionType;
        _nextCallbackAction.data = data;
    }

    function _executeBeforeCallbackAction()
        private view
        returns (bytes memory cbdata)
    {
        if (_nextCallbackAction.actionType == NextCallbackActionType.Noop) {
            //emit NoopEvent();
            return "Noop";
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Assert) {
            assert(false);
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Revert) {
            // solhint-disable-next-line reason-string
            revert();
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.RevertWithReason) {
            revert(abi.decode(_nextCallbackAction.data, (string)));
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.BurnGas) {
            uint256 gasToBurn = abi.decode(_nextCallbackAction.data, (uint256));
            _burnGas(gasToBurn);
        } else assert(false);
    }

    function _executeAfterCallbackAction(bytes memory ctx)
        private
        returns (bytes memory newCtx)
    {
        if (_nextCallbackAction.actionType == NextCallbackActionType.Noop) {
            emit NoopEvent();
            return ctx;
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Assert) {
            assert(false);
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Revert) {
            // solhint-disable-next-line reason-string
            revert();
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.RevertWithReason) {
            revert(abi.decode(_nextCallbackAction.data, (string)));
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.AlteringCtx) {
            return abi.encode(42);
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.BurnGas) {
            uint256 gasToBurn = abi.decode(_nextCallbackAction.data, (uint256));
            _burnGas(gasToBurn);
        } else assert(false);
    }

    function beforeAgreementCreated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        return _executeBeforeCallbackAction();
    }

    function afterAgreementCreated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        return _executeAfterCallbackAction(ctx);
    }

    function beforeAgreementUpdated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        return _executeBeforeCallbackAction();
    }

    function afterAgreementUpdated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        return _executeAfterCallbackAction(ctx);
    }

    function beforeAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata ctx
    )
        external view
        validCtx(ctx)
        virtual override
        returns (bytes memory /*cbdata*/)
    {
        return _executeBeforeCallbackAction();
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*cbdata*/,
        bytes calldata ctx
    )
        external
        validCtx(ctx)
        virtual override
        returns (bytes memory newCtx)
    {
        return _executeAfterCallbackAction(ctx);
    }

    function _burnGas(uint256 gasToBurn) private view {
        uint256 gasStart = gasleft();
        uint256 gasNow = gasleft();
        while ((gasStart - gasNow) < gasToBurn - 1000 /* some margin for other things*/) {
            gasNow = gasleft();
        }
    }

    modifier validCtx(bytes calldata ctx) {
        require(ISuperfluid(msg.sender).isCtxValid(ctx), "AgreementMock: ctx not valid before");
        _;
    }

}
