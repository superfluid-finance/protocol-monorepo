import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleFlowOperatorUpdated,
    handleFlowUpdated,
} from "../../src/mappings/cfav1";
import { BIG_INT_ZERO, getAccountTokenSnapshotID, getStreamID, ZERO_ADDRESS } from "../../src/utils";
import {
    assertEventBaseProperties,
    assertHigherOrderBaseProperties,
} from "../assertionHelper";
import { alice, bob, DEFAULT_DECIMALS, maticx } from "../constants";
import { stringToBytes } from "../converters";
import {
    createFlowOperatorUpdatedEvent,
    createFlowUpdatedEvent,
    getFlowOperatorId,
} from "./cfav1.helper";
import { mockedHandleFlowUpdatedRPCCalls } from "./cfav1.helper";

describe("ConstantFlowAgreementV1 Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleFlowUpdated() - Should create a new FlowUpdatedEvent entity (create)", () => {
            const superToken = maticx;
            const sender = alice;
            const receiver = bob;
            const flowRate = BigInt.fromI32(100);
            const totalSenderFlowRate = BigInt.fromI32(100);
            const totalReceiverFlowRate = BigInt.fromI32(100);
            const userData = stringToBytes("");
            const oldFlowRate = BigInt.fromI32(0);
            const type = "0";

            const flowUpdatedEvent = createFlowUpdatedEvent(
                superToken,
                sender,
                receiver,
                flowRate,
                totalSenderFlowRate,
                totalReceiverFlowRate,
                userData
            );

            mockedHandleFlowUpdatedRPCCalls(
                flowUpdatedEvent,
                superToken,
                DEFAULT_DECIMALS,
                "Super Matic",
                "MATICx",
                ZERO_ADDRESS,
                flowRate,
                BIG_INT_ZERO
            );

            handleFlowUpdated(flowUpdatedEvent);

            const id = assertEventBaseProperties(
                flowUpdatedEvent,
                "FlowUpdated"
            );
            const streamId = getStreamID(
                Address.fromString(sender),
                Address.fromString(receiver),
                Address.fromString(superToken),
                0
            );
            assert.fieldEquals("FlowUpdatedEvent", id, "token", superToken);
            assert.fieldEquals("FlowUpdatedEvent", id, "sender", sender);
            assert.fieldEquals("FlowUpdatedEvent", id, "receiver", receiver);
            assert.fieldEquals("FlowUpdatedEvent", id, "flowOperator", ZERO_ADDRESS.toHexString());
            assert.fieldEquals("FlowUpdatedEvent", id, "flowRate", flowRate.toString());
            assert.fieldEquals("FlowUpdatedEvent", id, "totalSenderFlowRate", totalSenderFlowRate.toString());
            assert.fieldEquals("FlowUpdatedEvent", id, "totalReceiverFlowRate", totalReceiverFlowRate.toString());
            assert.fieldEquals("FlowUpdatedEvent", id, "userData", userData.toHexString());
            assert.fieldEquals("FlowUpdatedEvent", id, "oldFlowRate", oldFlowRate.toString());
            assert.fieldEquals("FlowUpdatedEvent", id, "type", type);
            assert.fieldEquals("FlowUpdatedEvent", id, "stream", streamId);
        });

        test("handleFlowOperatorUpdated() - Should create a new FlowOperatorUpdatedEvent entity", () => {
            const superToken = maticx;
            const permissions = 1; // create only
            const flowRateAllowance = BigInt.fromI32(100);
            const sender = alice;
            const flowOperator = bob;

            const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
                superToken,
                sender,
                flowOperator,
                permissions,
                flowRateAllowance
            );

            handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

            const id = assertEventBaseProperties(
                flowOperatorUpdatedEvent,
                "FlowOperatorUpdated"
            );
            const flowOperatorId = getFlowOperatorId(flowOperator, superToken, sender);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "token", superToken);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "sender", sender);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "flowOperator", flowOperatorId);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "permissions", permissions.toString());
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "flowRateAllowance", flowRateAllowance.toString());
        });
    });

    describe("Higher Order Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleFlowOperatorUpdated() - Should create a new FlowOperator entity", () => {
            const superToken = maticx;
            const permissions = 1; // create only
            const flowRateAllowance = BigInt.fromI32(100);
            const sender = alice;
            const flowOperator = bob;

            const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
                superToken,
                sender,
                flowOperator,
                permissions,
                flowRateAllowance
            );

            handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

            const id = getFlowOperatorId(flowOperator, superToken, sender);
            const atsId = getAccountTokenSnapshotID(Address.fromString(sender), Address.fromString(superToken));
            assertHigherOrderBaseProperties(
                "FlowOperator",
                id,
                flowOperatorUpdatedEvent.block.timestamp,
                flowOperatorUpdatedEvent.block.number,
                flowOperatorUpdatedEvent.block.timestamp,
                flowOperatorUpdatedEvent.block.number
            );
            assert.fieldEquals("FlowOperator", id, "permissions", permissions.toString());
            assert.fieldEquals("FlowOperator", id, "flowRateAllowanceGranted", flowRateAllowance.toString());
            assert.fieldEquals("FlowOperator", id, "flowRateAllowanceRemaining", flowRateAllowance.toString());
            assert.fieldEquals("FlowOperator", id, "flowOperator", flowOperator);
            assert.fieldEquals("FlowOperator", id, "sender", sender);
            assert.fieldEquals("FlowOperator", id, "token", superToken);
            assert.fieldEquals("FlowOperator", id, "accountTokenSnapshot", atsId);
        });
    });
});
