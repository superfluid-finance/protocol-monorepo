import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import { handleFlowOperatorUpdated } from "../../src/mappings/cfav1";
import { getAccountTokenSnapshotID } from "../../src/utils";
import { assertEventBaseProperties, assertHigherOrderBaseProperties } from "../assertionHelper";
import {
    alice,
    bob,
    maticx
} from "../constants";
import { createFlowOperatorUpdatedEvent, getFlowOperatorId } from "./cfav1.helper";

describe("ConstantFlowAgreementV1 Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
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
