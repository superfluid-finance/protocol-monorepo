import { BigInt } from "@graphprotocol/graph-ts";
import { assert, beforeEach, clearStore, describe, test } from "matchstick-as/assembly/index";
import {
    handleBufferAdjusted,
    handleFlowDistributionUpdated,
    handleInstantDistributionUpdated,
    handlePoolConnectionUpdated,
} from "../../../src/mappings/gdav1";
import { handleDistributionClaimed, handleMemberUnitsUpdated } from "../../../src/mappings/superfluidPool";
import { BIG_INT_ZERO } from "../../../src/utils";
import { assertEventBaseProperties } from "../../assertionHelpers";
import { FAKE_INITIAL_BALANCE, FALSE, TRUE, alice, bob, maticXAddress, superfluidPool } from "../../constants";
import {
    createBufferAdjustedEvent,
    createDistributionClaimedEvent,
    createFlowDistributionUpdatedEvent,
    createInstantDistributionUpdatedEvent,
    createMemberUnitsUpdatedEvent,
    createPoolAndReturnPoolCreatedEvent,
    createPoolConnectionUpdatedEvent,
} from "../gdav1.helper";
import { mockedGetAppManifest, mockedRealtimeBalanceOf } from "../../mockedFunctions";

const initialFlowRate = BigInt.fromI32(100);
const superToken = maticXAddress;

describe("GeneralDistributionAgreementV1 Event Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handlePoolCreated() - Should create a new PoolCreatedEvent entity", () => {
        const admin = bob;
        const poolCreatedEvent = createPoolAndReturnPoolCreatedEvent(admin, superToken, superfluidPool);

        const id = assertEventBaseProperties(poolCreatedEvent, "PoolCreated");
        assert.fieldEquals("PoolCreatedEvent", id, "token", superToken);
        assert.fieldEquals("PoolCreatedEvent", id, "caller", poolCreatedEvent.transaction.from.toHexString());
        assert.fieldEquals("PoolCreatedEvent", id, "admin", admin);
        assert.fieldEquals("PoolCreatedEvent", id, "pool", superfluidPool);
    });

    test("handlePoolConnectionUpdated() - Should create a new handlePoolConnectionUpdatedEvent entity (connected)", () => {
        const account = bob;
        const connected = true;

        const poolConnectionUpdatedEvent = createPoolConnectionUpdatedEvent(
            superToken,
            superfluidPool,
            account,
            connected
        );

        // getOrInitAccountTokenSnapshot(event) => getOrInitAccount(account) => host.try_getAppManifest(account)
        mockedGetAppManifest(account, false, false, BIG_INT_ZERO);

        // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(account)
        mockedRealtimeBalanceOf(
            superToken,
            account,
            poolConnectionUpdatedEvent.block.timestamp,
            FAKE_INITIAL_BALANCE.plus(initialFlowRate),
            initialFlowRate,
            BIG_INT_ZERO
        );

        handlePoolConnectionUpdated(poolConnectionUpdatedEvent);

        const id = assertEventBaseProperties(poolConnectionUpdatedEvent, "PoolConnectionUpdated");
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "connected", TRUE);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "poolMember", account);
    });

    test("handlePoolConnectionUpdated() - Should create a new handlePoolConnectionUpdatedEvent entity (disconnected)", () => {
        const account = bob;
        const connected = false;

        const poolConnectionUpdatedEvent = createPoolConnectionUpdatedEvent(
            superToken,
            superfluidPool,
            account,
            connected
        );

        // getOrInitAccountTokenSnapshot(event) => getOrInitAccount(account) => host.try_getAppManifest(account)
        mockedGetAppManifest(account, false, false, BIG_INT_ZERO);

        // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(account)
        mockedRealtimeBalanceOf(
            superToken,
            account,
            poolConnectionUpdatedEvent.block.timestamp,
            FAKE_INITIAL_BALANCE.plus(initialFlowRate),
            initialFlowRate,
            BIG_INT_ZERO
        );

        handlePoolConnectionUpdated(poolConnectionUpdatedEvent);

        const id = assertEventBaseProperties(poolConnectionUpdatedEvent, "PoolConnectionUpdated");
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "connected", FALSE);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("PoolConnectionUpdatedEvent", id, "poolMember", account);
    });

    test("handleBufferAdjusted() - Should create a new handleBufferAdjustedEvent entity", () => {
        const bufferDelta = BigInt.fromI32(69);
        const newBufferAmount = BigInt.fromI32(420);
        const totalBufferAmount = BigInt.fromI32(42069);
        const poolDistributor = alice;

        const bufferAdjustedEvent = createBufferAdjustedEvent(
            maticXAddress,
            superfluidPool,
            poolDistributor,
            bufferDelta,
            newBufferAmount,
            totalBufferAmount
        );

        handleBufferAdjusted(bufferAdjustedEvent);

        const id = assertEventBaseProperties(bufferAdjustedEvent, "BufferAdjusted");
        assert.fieldEquals("BufferAdjustedEvent", id, "token", maticXAddress);
        assert.fieldEquals("BufferAdjustedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("BufferAdjustedEvent", id, "poolDistributor", poolDistributor);
        assert.fieldEquals("BufferAdjustedEvent", id, "bufferDelta", bufferDelta.toString());
        assert.fieldEquals("BufferAdjustedEvent", id, "newBufferAmount", newBufferAmount.toString());
        assert.fieldEquals("BufferAdjustedEvent", id, "totalBufferAmount", totalBufferAmount.toString());
    });

    test("handleInstantDistributionUpdated() - Should create a new handleInstantDistributionUpdatedEvent entity", () => {
        const operator = alice;
        const requestedAmount = BigInt.fromI32(69);
        const actualAmount = BigInt.fromI32(70);
        const poolDistributor = bob;

        const instantDistributionUpdatedEvent = createInstantDistributionUpdatedEvent(
            superToken,
            superfluidPool,
            poolDistributor,
            operator,
            requestedAmount,
            actualAmount
        );

        handleInstantDistributionUpdated(instantDistributionUpdatedEvent);

        const id = assertEventBaseProperties(instantDistributionUpdatedEvent, "InstantDistributionUpdated");
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "poolDistributor", poolDistributor);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "operator", operator);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "requestedAmount", requestedAmount.toString());
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "actualAmount", actualAmount.toString());
    });

    test("handleFlowDistributionUpdated() - Should create a new handleFlowDistributionUpdatedEvent entity", () => {
        const operator = alice;
        const oldFlowRate = BigInt.fromI32(69);
        const newDistributorToPoolFlowRate = BigInt.fromI32(420);
        const newTotalDistributionFlowRate = BigInt.fromI32(42069);
        const adjustmentFlowRecipient = alice;
        const adjustmentFlowRate = BigInt.fromI32(5);
        const poolDistributor = bob;

        const flowDistributionUpdatedEvent = createFlowDistributionUpdatedEvent(
            superToken,
            superfluidPool,
            poolDistributor,
            operator,
            oldFlowRate,
            newDistributorToPoolFlowRate,
            newTotalDistributionFlowRate,
            adjustmentFlowRecipient,
            adjustmentFlowRate
        );

        handleFlowDistributionUpdated(flowDistributionUpdatedEvent);

        const id = assertEventBaseProperties(flowDistributionUpdatedEvent, "FlowDistributionUpdated");
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "poolDistributor", poolDistributor);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "operator", operator);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "oldFlowRate", oldFlowRate.toString());
        assert.fieldEquals(
            "FlowDistributionUpdatedEvent",
            id,
            "newDistributorToPoolFlowRate",
            newDistributorToPoolFlowRate.toString()
        );
        assert.fieldEquals(
            "FlowDistributionUpdatedEvent",
            id,
            "newTotalDistributionFlowRate",
            newTotalDistributionFlowRate.toString()
        );
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "adjustmentFlowRecipient", adjustmentFlowRecipient);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "adjustmentFlowRate", adjustmentFlowRate.toString());
    });

    test("handleDistributionClaimed() - Should create a new DistributionClaimedEvent entity", () => {
        const poolMember = alice;
        const claimedAmount = BigInt.fromI32(69);
        const totalClaimed = BigInt.fromI32(420);

        const distributionClaimedEvent = createDistributionClaimedEvent(
            superToken,
            poolMember,
            claimedAmount,
            totalClaimed
        );

        // getOrInitAccountTokenSnapshot(event) => getOrInitAccount(account) => host.try_getAppManifest(account)
        mockedGetAppManifest(poolMember, false, false, BIG_INT_ZERO);

        // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(poolMember)
        mockedRealtimeBalanceOf(
            superToken,
            poolMember,
            distributionClaimedEvent.block.timestamp,
            FAKE_INITIAL_BALANCE.plus(initialFlowRate),
            initialFlowRate,
            BIG_INT_ZERO
        );

        handleDistributionClaimed(distributionClaimedEvent);

        const id = assertEventBaseProperties(distributionClaimedEvent, "DistributionClaimed");
        assert.fieldEquals("DistributionClaimedEvent", id, "token", superToken);
        assert.fieldEquals("DistributionClaimedEvent", id, "claimedAmount", claimedAmount.toString());
        assert.fieldEquals("DistributionClaimedEvent", id, "totalClaimed", totalClaimed.toString());
        assert.fieldEquals("DistributionClaimedEvent", id, "poolMember", poolMember);
    });

    test("handleMemberUnitsUpdated() - Should create a new MemberUnitsUpdatedEvent entity", () => {
        const units = BigInt.fromI32(69);
        const poolMember = bob;

        const memberUnitsUpdatedEvent = createMemberUnitsUpdatedEvent(superToken, poolMember, units);

        handleMemberUnitsUpdated(memberUnitsUpdatedEvent);

        const id = assertEventBaseProperties(memberUnitsUpdatedEvent, "MemberUnitsUpdated");
        assert.fieldEquals("MemberUnitsUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("MemberUnitsUpdatedEvent", id, "poolMember", poolMember);
        assert.fieldEquals("MemberUnitsUpdatedEvent", id, "units", units.toString());
    });
});
