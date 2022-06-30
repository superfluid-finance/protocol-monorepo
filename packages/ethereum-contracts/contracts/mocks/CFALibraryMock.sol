// SPDX-License-Identifier: MIT
pragma solidity 0.8.14;

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
        cfaV1.createFlow(receiver, token, flowRate);
    }

    function updateFlowTest(
        ISuperfluidToken token,
        address receiver,
        int96 flowRate
    ) public {
        cfaV1.updateFlow(receiver, token, flowRate);
    }

    function deleteFlowTest(ISuperfluidToken token, address receiver) public {
        cfaV1.deleteFlow(address(this), receiver, token);
    }

    function createFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.createFlowByOperator(sender, receiver, token, flowRate);
    }

    function updateFlowByOperatorTest(
        address sender,
        address receiver,
        ISuperfluidToken token,
        int96 flowRate
    ) public {
        cfaV1.updateFlowByOperator(sender, receiver, token, flowRate);
    }

    function deleteFlowByOperator(
        address sender,
        address receiver,
        ISuperfluidToken token
    ) public {
        cfaV1.deleteFlowByOperator(sender, receiver, token);
    }

    function updateFlowOperatorPermissionsTest(
        address flowOperator,
        ISuperfluidToken token,
        uint8 permissions,
        int96 flowRateAllowance
    ) public {
        cfaV1.updateFlowOperatorPermissions(flowOperator, token, permissions, flowRateAllowance);
    }

    function authorizeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.authorizeFlowOperatorWithFullControl(flowOperator, token);
    }

    function revokeFlowOperatorWithFullControlTest(
        address flowOperator,
        ISuperfluidToken token
    ) public {
        cfaV1.revokeFlowOperatorWithFullControl(flowOperator, token);
    }
}

contract CFALibrarySuperAppMock is SuperAppBase {

    using CFAv1Library for CFAv1Library.InitData;
    CFAv1Library.InitData internal cfaV1;

    // default values for smoke tests
    uint8 internal immutable permissions = 7;
    int96 internal immutable flowRate = 1000000000000;
    int96 internal immutable updatedFlowRate = 2000000000000;
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
        cfaV1.createFlow(receiver, token, flowRate);
    }

    // literally ONLY for the revokeFlowOperatorWithFullControlWithCtx test.
    function authorizeFlowOperatorWithFullControl(ISuperToken token) external {
        cfaV1.authorizeFlowOperatorWithFullControl(flowOperator, token);
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
            return cfaV1.createFlowWithCtx(ctx, receiver, token, flowRate);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW))
            return cfaV1.updateFlowWithCtx(ctx, receiver, token, updatedFlowRate);
        else if (functionIndex == uint8(FunctionIndex.DELETE_FLOW))
            return cfaV1.deleteFlowWithCtx(ctx, address(this), receiver, token);
        else if (functionIndex == uint8(FunctionIndex.CREATE_FLOW_BY_OPERATOR))
            return cfaV1.createFlowByOperatorWithCtx(ctx, sender, receiver, token, flowRate);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_BY_OPERATOR))
            return cfaV1.updateFlowByOperatorWithCtx(ctx, sender, receiver, token, updatedFlowRate);
        else if (functionIndex == uint8(FunctionIndex.DELETE_FLOW_BY_OPERATOR))
            return cfaV1.deleteFlowByOperatorWithCtx(ctx, sender, receiver, token);
        else if (functionIndex == uint8(FunctionIndex.UPDATE_FLOW_OPERATOR_PERMISSIONS))
            return cfaV1.updateFlowOperatorPermissionsWithCtx(
                ctx,
                flowOperator,
                token,
                permissions,
                flowRate
            );
        else if (functionIndex == uint8(FunctionIndex.AUTHORIZE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.authorizeFlowOperatorWithFullControlWithCtx(ctx, flowOperator, token);
        else if (functionIndex == uint8(FunctionIndex.REVOKE_FLOW_OPERATOR_WITH_FULL_CONTROL))
            return cfaV1.revokeFlowOperatorWithFullControlWithCtx(ctx, flowOperator, token);
        else revert("invalid function index");
    }
}
