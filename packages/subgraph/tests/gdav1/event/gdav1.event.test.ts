import { BigInt } from "@graphprotocol/graph-ts";
import { assert, beforeEach, clearStore, describe, test } from "matchstick-as/assembly/index";
import {
    handleBufferAdjusted,
    handleFlowDistributionUpdated,
    handleInstantDistributionUpdated,
    handlePoolConnectionUpdated,
    handlePoolCreated,
} from "../../../src/mappings/gdav1";
import { BIG_INT_ZERO } from "../../../src/utils";
import { assertEventBaseProperties } from "../../assertionHelpers";
import { FAKE_INITIAL_BALANCE, FALSE, TRUE, alice, bob, maticXAddress, superfluidPool } from "../../constants";
import {
    createBufferAdjustedEvent,
    createDistributionClaimedEvent,
    createFlowDistributionUpdatedEvent,
    createInstantDistributionUpdatedEvent,
    createMemberUnitsUpdatedEvent,
    createPoolConnectionUpdatedEvent,
    createPoolCreatedEvent,
} from "../gdav1.helper";
import { mockedGetAppManifest, mockedRealtimeBalanceOf } from "../../mockedFunctions";

const initialFlowRate = BigInt.fromI32(100);
const pool = maticXAddress;

describe("GeneralDistributionAgreementV1 Event Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handlePoolCreated() - Should create a new PoolCreatedEvent entity", () => {
        const superToken = maticXAddress;
        const admin = bob;

        const poolCreatedEvent = createPoolCreatedEvent(superToken, admin, superfluidPool);

        // getOrInitAccountTokenSnapshot(event) => getOrInitAccount(admin) => host.try_getAppManifest(admin)
        mockedGetAppManifest(admin, false, false, BIG_INT_ZERO);

        // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(admin)
        mockedRealtimeBalanceOf(
            superToken,
            admin,
            poolCreatedEvent.block.timestamp,
            FAKE_INITIAL_BALANCE.plus(initialFlowRate),
            initialFlowRate,
            BIG_INT_ZERO
        );

        handlePoolCreated(poolCreatedEvent);

        const id = assertEventBaseProperties(poolCreatedEvent, "PoolCreated");
        assert.fieldEquals("PoolCreatedEvent", id, "token", superToken);
        assert.fieldEquals("PoolCreatedEvent", id, "caller", poolCreatedEvent.transaction.from.toHexString());
        assert.fieldEquals("PoolCreatedEvent", id, "admin", admin);
        assert.fieldEquals("PoolCreatedEvent", id, "pool", superfluidPool);
    });

    test("handlePoolConnectionUpdated() - Should create a new handlePoolConnectionUpdatedEvent entity (connected)", () => {
        const superToken = maticXAddress;
        const account = bob;
        const connected = true;

        const poolConnectionUpdatedEvent = createPoolConnectionUpdatedEvent(
            superToken,
            connected,
            superfluidPool,
            account
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
        const superToken = maticXAddress;
        const account = bob;
        const connected = false;

        const poolConnectionUpdatedEvent = createPoolConnectionUpdatedEvent(
            superToken,
            connected,
            superfluidPool,
            account
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
            bufferDelta,
            newBufferAmount,
            totalBufferAmount,
            superfluidPool,
            poolDistributor
        );

        handleBufferAdjusted(bufferAdjustedEvent);

        const id = assertEventBaseProperties(bufferAdjustedEvent, "BufferAdjusted");
        assert.fieldEquals("BufferAdjustedEvent", id, "token", maticXAddress);
        assert.fieldEquals("BufferAdjustedEvent", id, "bufferDelta", bufferDelta.toString());
        assert.fieldEquals("BufferAdjustedEvent", id, "newBufferAmount", newBufferAmount.toString());
        assert.fieldEquals("BufferAdjustedEvent", id, "totalBufferAmount", totalBufferAmount.toString());
        assert.fieldEquals("BufferAdjustedEvent", id, "pool", superfluidPool);
    });

    test("handleInstantDistributionUpdated() - Should create a new handleInstantDistributionUpdatedEvent entity", () => {
        const superToken = maticXAddress;
        const operator = alice;
        const requestedAmount = BigInt.fromI32(69);
        const actualAmount = BigInt.fromI32(70);
        const poolDistributor = bob;

        const instantDistributionUpdatedEvent = createInstantDistributionUpdatedEvent(
            superToken,
            operator,
            requestedAmount,
            actualAmount,
            superfluidPool,
            poolDistributor
        );

        handleInstantDistributionUpdated(instantDistributionUpdatedEvent);

        const id = assertEventBaseProperties(instantDistributionUpdatedEvent, "InstantDistributionUpdated");
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "token", superToken);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "operator", operator);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "requestedAmount", requestedAmount.toString());
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "actualAmount", actualAmount.toString());
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("InstantDistributionUpdatedEvent", id, "poolDistributor", poolDistributor);
    });

    test("handleFlowDistributionUpdated() - Should create a new handleFlowDistributionUpdatedEvent entity", () => {
        const superToken = maticXAddress;
        const operator = alice;
        const oldFlowRate = BigInt.fromI32(69);
        const newDistributorToPoolFlowRate = BigInt.fromI32(420);
        const newTotalDistributionFlowRate = BigInt.fromI32(42069);
        const adjustmentFlowRecipient = alice;
        const adjustmentFlowRate = BigInt.fromI32(5);
        const poolDistributor = bob;

        const flowDistributionUpdatedEvent = createFlowDistributionUpdatedEvent(
            superToken,
            operator,
            oldFlowRate,
            newDistributorToPoolFlowRate,
            newTotalDistributionFlowRate,
            adjustmentFlowRecipient,
            adjustmentFlowRate,
            superfluidPool,
            poolDistributor
        );

        handleFlowDistributionUpdated(flowDistributionUpdatedEvent);

        const id = assertEventBaseProperties(flowDistributionUpdatedEvent, "FlowDistributionUpdated");
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "token", superToken);
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
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "pool", superfluidPool);
        assert.fieldEquals("FlowDistributionUpdatedEvent", id, "poolDistributor", poolDistributor);
    });
});
