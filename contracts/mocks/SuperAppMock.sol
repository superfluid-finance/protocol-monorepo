// SPDX-License-Identifier: MIT
pragma solidity 0.7.4;

import {
    ISuperfluid,
    ISuperToken,
    ISuperApp,
    ISuperAgreement
} from "../superfluid/Superfluid.sol";

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

    function actionNoop(bytes memory /* ctx */) external {
        emit NoopEvent();
    }

    function actionAssert(bytes memory /* ctx */) external pure {
        assert(false);
    }

    function actionRevert(bytes memory /* ctx */) external pure {
        // solhint-disable-next-line reason-string
        revert();
    }

    function actionRevertWithReason(string memory reason, bytes memory /* ctx */) external pure {
        revert(reason);
    }

    function actionCallAgreement(bytes memory /* ctx */) external {
        // this should fail, action should call agreement with ctx
        _host.callAgreement(ISuperAgreement(address(0)), "");
    }

    function actionCallAppAction(bytes memory /* ctx */) external {
        // this should fail, action should call agreement with ctx
        _host.callAppAction(ISuperApp(address(0)), "");
    }

    /*************************************************************************
    * Callbacks
    **************************************************************************/

    enum NextCallbackActionType {
        Noop, // 0
        Assert, // 1
        Revert, // 2
        RevertWithReason // 3
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
        private view returns (bytes memory cbdata)
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
        } else assert(false);
    }

    function _executeAfterCallbackAction()
        private
    {
        if (_nextCallbackAction.actionType == NextCallbackActionType.Noop) {
            emit NoopEvent();
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Assert) {
            assert(false);
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.Revert) {
            // solhint-disable-next-line reason-string
            revert();
        } else if (_nextCallbackAction.actionType == NextCallbackActionType.RevertWithReason) {
            revert(abi.decode(_nextCallbackAction.data, (string)));
        } else assert(false);
    }

    function beforeAgreementCreated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*ctx*/
    )
        external
        view
        virtual
        override
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
        virtual
        override
        returns (bytes memory newCtx)
    {
        _executeAfterCallbackAction();
        newCtx = ctx;
    }

    function beforeAgreementUpdated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*ctx*/
    )
        external
        view
        virtual
        override
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
        virtual
        override
        returns (bytes memory newCtx)
    {
        _executeAfterCallbackAction();
        newCtx = ctx;
    }

    function beforeAgreementTerminated(
        ISuperToken /*superToken*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*agreementData*/,
        bytes calldata /*ctx*/
    )
        external
        view
        virtual
        override
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
        virtual
        override
        returns (bytes memory newCtx)
    {
        _executeAfterCallbackAction();
        newCtx = ctx;
    }

}
