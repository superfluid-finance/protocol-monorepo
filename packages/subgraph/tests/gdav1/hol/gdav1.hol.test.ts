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
import {
    FAKE_INITIAL_BALANCE,
    alice,
    bob,
    maticXAddress,
    maticXName,
    maticXSymbol,
    superfluidPool,
} from "../../constants";
import {
    createBufferAdjustedEvent,
    createDistributionClaimedEvent,
    createFlowDistributionUpdatedEvent,
    createInstantDistributionUpdatedEvent,
    createMemberUnitsUpdatedEvent,
    createPoolConnectionUpdatedEvent,
    createPoolCreatedEvent,
} from "../gdav1.helper";
import {
    handlePoolCreated
} from "../../../src/mappings/gdav1";
import { mockedApprove, mockedGetAppManifest, mockedRealtimeBalanceOf } from "../../mockedFunctions";
import { Pool } from "../../../generated/schema";
import { getDeposit } from "../../cfav1/cfav1.helper";

const initialFlowRate = BigInt.fromI32(100);
const superToken = maticXAddress;

describe("GeneralDistributionAgreementV1 Higher Order Level Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handlePoolCreated() - Should create a new Pool entity (create)", () => {
        const admin = alice;
        // create pool
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

        const id = poolCreatedEvent.params.pool.toHexString();
        assertHigherOrderBaseProperties("Pool", id, poolCreatedEvent);
        assert.fieldEquals("Pool", id, "totalUnits", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalConnectedUnits", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalDisconnectedUnits", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalAmountInstantlyDistributedUntilUpdatedAt", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalAmountFlowedDistributedUntilUpdatedAt", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalAmountDistributedUntilUpdatedAt", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalMembers", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalConnectedMembers", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalDisconnectedMembers", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "adjustmentFlowRate", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "totalBuffer", BIG_INT_ZERO.toString());
        assert.fieldEquals("Pool", id, "token", poolCreatedEvent.params.token.toHexString());
        assert.fieldEquals("Pool", id, "admin", admin);
    });

    // Pool
    // handlePoolConnectionUpdated
    // handleBufferAdjusted
    // handleFlowDistributionUpdated
    // handleInstantDistributionUpdated
    // handleDistributionClaimed
    // handleMemberUnitsUpdated
    // connected member
    // disconnected member
    // 0 => some units
    // connected member
    // disconnected member
    // some units => 0
    // connected member
    // disconnected member

    // PoolMember
    // handlePoolConnectionUpdated
    // handleDistributionClaimed
    // handleMemberUnitsUpdated
    // PoolDistributor
    // handleBufferAdjusted
    // handleFlowDistributionUpdated
    // handleInstantDistributionUpdated
});
