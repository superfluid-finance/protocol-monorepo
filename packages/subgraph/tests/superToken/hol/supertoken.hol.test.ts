import { assert, beforeEach, clearStore, describe, test } from "matchstick-as";
import {
    createApprovalEvent,
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

        mockedApprove(superToken, sender, flowOperator, allowance);

        const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
            superToken,
            sender,
            flowOperator,
            permissions,
            flowRateAllowance
        );

        handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

        const approvalEvent = createApprovalEvent(
            superToken,
            sender,
            flowOperator,
            allowance
        );

        handleApproval(approvalEvent);

        const id = getFlowOperatorID(
            Address.fromString(flowOperator),
            Address.fromString(superToken),
            Address.fromString(sender)
        );
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
