// SPDX-License-Identifier: MIT
pragma solidity 0.8.16;

import {ISuperfluid, ISuperfluidToken, ISuperToken} from "../interfaces/superfluid/ISuperfluid.sol";

import {ISuperApp, ISuperAgreement} from "../interfaces/superfluid/ISuperfluid.sol";

import {ContextDefinitions, SuperAppDefinitions} from "../interfaces/superfluid/ISuperfluid.sol";

import {SuperAppBase} from "../apps/SuperAppBase.sol";

import {IConstantFlowAgreementV1} from "../interfaces/agreements/IConstantFlowAgreementV1.sol";

import {CFAv1Library} from "../apps/CFAv1Library.sol";

contract CFALibraryMock {
    using CFAv1Library for CFAv1Library.InitData;

    //initialize cfaV1 variable
    CFAv1Library.InitData public cfaV1;

    constructor(ISuperfluid host) {
        //initialize InitData struct, and set equal to cfaV1
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256(
                            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                        )
                    )
                )
            )
        );
    }

    function createFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.createFlow(token, receiver, flowRate);
    }

    function updateFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.updateFlow(token, receiver, flowRate);
    }

    function deleteFlowTest(ISuperfluidToken token, address receiver) public {
        cfaV1.deleteFlow(token, address(this), receiver);
    }

    function createFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.createFlowByOperator(token, sender, receiver, flowRate);
    }

    function updateFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.updateFlowByOperator(token, sender, receiver, flowRate);
    }

    function deleteFlowByOperator(
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlowByOperator(token, sender, receiver);
    }

    function updateFlowOperatorPermissionsTest(
        address flowOperator,
        ISuperfluidToken token,
        uint8 permissions,
        int96 flowRateAllowance
    ) public {
        cfaV1.updateFlowOperatorPermissions(token, flowOperator, permissions, flowRateAllowance);
    }

    function authorizeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.authorizeFlowOperatorWithFullControl(token, flowOperator);
    }

    function revokeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.revokeFlowOperatorWithFullControl(token, flowOperator);
    }
}

contract CFALibrarySuperAppMock is SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData internal cfaV1;

    // default values for smoke tests
    uint8 internal constant PERMISSIONS = 7;
    int96 internal constant FLOW_RATE = 1000000000000;
    int96 internal constant UPDATED_FLOW_RATE = 2000000000000;
    address internal immutable sender;
    address internal immutable receiver;
    address internal immutable flowOperator;

    // for selectively testing functions in the same callback
    enum FunctionIndex {
        CREATE_FLOW,
        UPDATE_FLOW,
        DELETE_FLOW,
        CREATE_FLOW_BY_OPERATOR,
        UPDATE_FLOW_BY_OPERATOR,
        DELETE_FLOW_BY_OPERATOR,
        UPDATE_FLOW_OPERATOR_PERMISSIONS,
        AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL,
        REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL
    }

    constructor(
        ISuperfluid host,
        address defaultSender,
        address defaultReceiver,
        address defaultFlowOperator
    ) {
        cfaV1 = CFAv1Library.InitData(
            host,
            IConstantFlowAgreementV1(
                address(
                    host.getAgreementClass(
                        keccak256(
                            "org.superfluid-finance.agreements.ConstantFlowAgreement.v1"
                        )
                    )
                )
            )
        );
        sender = defaultSender;
        receiver = defaultReceiver;
        flowOperator = defaultFlowOperator;

        uint256 configWord = SuperAppDefinitions.APP_LEVEL_FINAL |
            SuperAppDefinitions.BEFORE_AGREEMENT_CREATED_NOOP |
            // SuperAppDefinitions.AFTER_AGREEMENT_CREATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_UPDATED_NOOP |
            SuperAppDefinitions.BEFORE_AGREEMENT_TERMINATED_NOOP |
            SuperAppDefinitions.AFTER_AGREEMENT_TERMINATED_NOOP;

        host.registerAppWithKey(configWord, "");
    }

    function createFlow(ISuperToken token) external {
        cfaV1.createFlow(token, receiver, FLOW_RATE);
    }

    // literally ONLY for the revokeFlowOperatorWithFullControlWithCtx test.
    function authorizeFlowOperatorWithFullControl(ISuperToken token) external {
        cfaV1.authorizeFlowOperatorWithFullControl(token, flowOperator);
    }

    function afterAgreementCreated(
        ISuperToken token,
        address,
        bytes32,
        bytes calldata,
        bytes calldata,
        bytes calldata ctx
    ) external override returns (bytes memory) {
        bytes memory userData = cfaV1.host.decodeCtx(ctx).userData;
        (uint8 functionIndex) = abi.decode(userData, (uint8));

        if (functionIndex == uint8(FunctionIndex.CREATE_FLOW))
            return cfaV1.createFlowWithCtx(token, receiver, FLOW_RATE, ctx);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW))
            return cfaV1.updateFlowWithCtx(token, receiver, UPDATED_FLOW_RATE, ctx);
        else if (functionIndex == uint8(FunctionIndex.DELETE_FLOW))
            return cfaV1.deleteFlowWithCtx(token, address(this), receiver, ctx);
        else if (functionIndex == uint8(FunctionIndex.CREATE_FLOW_BY_OPERATOR))
            return cfaV1.createFlowByOperatorWithCtx(token, sender, receiver, FLOW_RATE, ctx);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_BY_OPERATOR))
            return cfaV1.updateFlowByOperatorWithCtx(token, sender, receiver, UPDATED_FLOW_RATE, ctx);
        else if (functionIndex == uint8(FunctionIndex.DELETE_FLOW_BY_OPERATOR))
            return cfaV1.deleteFlowByOperatorWithCtx(token, sender, receiver, ctx);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_OPERATOR_PERMISSIONS))
            return cfaV1.updateFlowOperatorPermissionsWithCtx(
                token,
                flowOperator,
                PERMISSIONS,
                FLOW_RATE,
                ctx
            );
        else if (functionIndex == uint8(FunctionIndex.AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.authorizeFlowOperatorWithFullControlWithCtx(token, flowOperator, ctx);
        else if (functionIndex == uint8(FunctionIndex.REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.revokeFlowOperatorWithFullControlWithCtx(token, flowOperator, ctx);
        else revert("invalid function index");
    }
}
