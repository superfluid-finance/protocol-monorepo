import { Address, BigInt } from "@graphprotocol/graph-ts";
import { assert, beforeEach, clearStore, describe, test } from "matchstick-as/assembly/index";
import { handleFlowOperatorUpdated } from "../../../src/mappings/cfav1";
import {
    BIG_INT_ZERO,
    getAccountTokenSnapshotID,
    getFlowOperatorID,
    getStreamID,
    ZERO_ADDRESS,
} from "../../../src/utils";
import { assertHigherOrderBaseProperties } from "../../assertionHelpers";
import { alice, bob, maticXAddress, maticXName, maticXSymbol } from "../../constants";
import {
    createFlowOperatorUpdatedEvent,
    getDeposit,
    modifyFlowAndAssertFlowUpdatedEventProperties,
} from "../cfav1.helper";
import { mockedApprove } from "../../mockedFunctions";

const initialFlowRate = BigInt.fromI32(100);

describe("ConstantFlowAgreementV1 Higher Order Level Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handleFlowUpdated() - Should create a new Stream entity (create)", () => {
        // create flow
        const flowUpdatedEvent = modifyFlowAndAssertFlowUpdatedEventProperties(
            maticXAddress,      // superToken
            maticXName,         // tokenName
            maticXSymbol,       // tokenSymbol
            alice,              // sender
            bob,                // receiver
            ZERO_ADDRESS,       // underlyingToken
            "0",                // expectedType
            BIG_INT_ZERO,       // expectedOwedDeposit
            initialFlowRate,    // flowRate
            BIG_INT_ZERO,       // previousSenderFlowRate
            BIG_INT_ZERO,       // previousReceiverFlowRate
            "henlo"             // userData
        );

        const id = getStreamID(
            flowUpdatedEvent.params.sender,
            flowUpdatedEvent.params.receiver,
            flowUpdatedEvent.params.token,
            0
        );
        const deposit = getDeposit(flowUpdatedEvent.params.flowRate);
        const streamedUntilUpdatedAt = _getStreamedUntilUpdatedAt(
            BIG_INT_ZERO,
            flowUpdatedEvent.block.timestamp,
            BIG_INT_ZERO,
            BIG_INT_ZERO
        );

        assertHigherOrderBaseProperties("Stream", id, flowUpdatedEvent);
        assert.fieldEquals("Stream", id, "currentFlowRate", flowUpdatedEvent.params.flowRate.toString());
        assert.fieldEquals("Stream", id, "deposit", deposit.toString());
        assert.fieldEquals("Stream", id, "streamedUntilUpdatedAt", streamedUntilUpdatedAt.toString());
        assert.fieldEquals("Stream", id, "token", flowUpdatedEvent.params.token.toHexString());
        assert.fieldEquals("Stream", id, "sender", flowUpdatedEvent.params.sender.toHexString());
        assert.fieldEquals("Stream", id, "receiver", flowUpdatedEvent.params.receiver.toHexString());
        assert.fieldEquals("Stream", id, "userData", flowUpdatedEvent.params.userData.toHexString());
    });

    test("handleFlowOperatorUpdated() - Should create a new FlowOperator entity", () => {
        const superToken = maticXAddress;
        const permissions = 1; // create only
        const flowRateAllowance = BigInt.fromI32(100);
        const allowance = BigInt.fromI32(0);
        const sender = alice;
        const flowOperator = bob;

        mockedApprove(superToken, sender, flowOperator, allowance);
        // Mocking is required here since it calls RPC inside getOrInitFlowOperator, when it is null.

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
        const atsId = getAccountTokenSnapshotID(Address.fromString(sender), Address.fromString(superToken));
        assertHigherOrderBaseProperties("FlowOperator", id, flowOperatorUpdatedEvent);
        assert.fieldEquals("FlowOperator", id, "permissions", permissions.toString());
        assert.fieldEquals("FlowOperator", id, "flowRateAllowanceGranted", flowRateAllowance.toString());
        assert.fieldEquals("FlowOperator", id, "flowRateAllowanceRemaining", flowRateAllowance.toString());
        assert.fieldEquals("FlowOperator", id, "flowOperator", flowOperator);
        assert.fieldEquals("FlowOperator", id, "allowance", allowance.toString());
        assert.fieldEquals("FlowOperator", id, "sender", sender);
        assert.fieldEquals("FlowOperator", id, "token", superToken);
        assert.fieldEquals("FlowOperator", id, "accountTokenSnapshot", atsId);
    });
});

/**
 * Calculates the streamedUntilUpdatedAt.
 * @param streamedSoFar
 * @param currentTime
 * @param lastUpdatedAtTime
 * @param previousOutflowRate
 * @returns streamedUntilUpdatedAt at lastUpdatedAtTime timestamp
 */
function _getStreamedUntilUpdatedAt(
    streamedSoFar: BigInt,
    currentTime: BigInt,
    lastUpdatedAtTime: BigInt,
    previousOutflowRate: BigInt
): BigInt {
    return streamedSoFar.plus(previousOutflowRate.times(currentTime.minus(lastUpdatedAtTime)));
}
