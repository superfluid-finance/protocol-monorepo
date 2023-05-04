import { assert, beforeEach, clearStore, describe, test } from "matchstick-as";
import {
    createFlowOperatorUpdatedEvent,
} from "../../cfav1/cfav1.helper";
import {
    alice,
    bob,
    maticXAddress,
} from "../../constants";
import {
    getAccountTokenSnapshotID,
    getFlowOperatorID,
} from "../../../src/utils";
import { Address, BigInt } from "@graphprotocol/graph-ts";
import { mockedApprove } from "../../mockedFunctions";
import { handleFlowOperatorUpdated } from "../../../src/mappings/cfav1";
import { handleApproval } from "../../../src/mappings/superToken";
import { assertHigherOrderBaseProperties } from "../../assertionHelpers";
import { createApprovalEvent } from "../superToken.helper";

describe("SuperToken Higher Order Level Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handleApproval() - Should create a new FlowOperator entity, approve flowOperator as spender", () => {
        const superToken = maticXAddress;
        const permissions = 1; // create only
        const flowRateAllowance = BigInt.fromI32(100);
        const allowance = BigInt.fromI32(100);
        const sender = alice;
        const flowOperator = bob;

        mockedApprove(superToken, sender, flowOperator, BigInt.fromI32(0));

        const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
            superToken,
            sender,
            flowOperator,
            permissions,
            flowRateAllowance
        );

        handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

        const id = getFlowOperatorID(
            Address.fromString(flowOperator),
            Address.fromString(superToken),
            Address.fromString(sender)
        );
        // check allowance is 0
        assert.fieldEquals("FlowOperator", id, "allowance", "0");

        // trigger approve event
        const approvalEvent = createApprovalEvent(
            superToken,
            sender,
            flowOperator,
            allowance
        );

        handleApproval(approvalEvent);

        const atsId = getAccountTokenSnapshotID(
            Address.fromString(sender),
            Address.fromString(superToken)
        );
        assertHigherOrderBaseProperties(
            "FlowOperator",
            id,
            flowOperatorUpdatedEvent.block.timestamp,
            flowOperatorUpdatedEvent.block.number,
            flowOperatorUpdatedEvent.block.timestamp,
            flowOperatorUpdatedEvent.block.number
        );
        assert.fieldEquals("FlowOperator", id, "permissions", permissions.toString()
        );
        assert.fieldEquals("FlowOperator", id, "flowRateAllowanceGranted", flowRateAllowance.toString());
        assert.fieldEquals("FlowOperator", id, "flowRateAllowanceRemaining", flowRateAllowance.toString()
        );
        assert.fieldEquals("FlowOperator", id, "flowOperator", flowOperator);
        assert.fieldEquals("FlowOperator", id, "allowance", allowance.toString()
        );
        assert.fieldEquals("FlowOperator", id, "sender", sender);
        assert.fieldEquals("FlowOperator", id, "token", superToken);
        assert.fieldEquals("FlowOperator", id, "accountTokenSnapshot", atsId);
    });
});
