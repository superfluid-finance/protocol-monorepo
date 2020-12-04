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

    event ActionNoopEvent();

    function actionNoop(bytes memory /* ctx */) external {
        emit ActionNoopEvent();
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
    function beforeAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback - Before Agreement Created");
    }

    function afterAgreementCreated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Created");
    }

    function beforeAgreementUpdated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback - Before Agreement updated");
    }

    function afterAgreementUpdated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes calldata /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Updated");
    }

    function beforeAgreementTerminated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/
    )
        external
        view
        virtual
        override
        returns (bytes memory /*cbdata*/)
    {
        revert("Unsupported callback -  Before Agreement Terminated");
    }

    function afterAgreementTerminated(
        ISuperToken /*superToken*/,
        bytes calldata /*ctx*/,
        address /*agreementClass*/,
        bytes32 /*agreementId*/,
        bytes memory /*cbdata*/
    )
        external
        virtual
        override
        returns (bytes memory /*newCtx*/)
    {
        revert("Unsupported callback - After Agreement Terminated");
    }

}
