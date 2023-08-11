import { Address, BigInt } from "@graphprotocol/graph-ts";
import { assert, beforeEach, clearStore, describe, test } from "matchstick-as/assembly/index";
import { handleFlowOperatorUpdated } from "../../../src/mappings/cfav1";
import {
    BIG_INT_ONE,
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
    createPoolAndReturnPoolCreatedEvent,
    createPoolConnectionUpdatedEvent,
    createPoolCreatedEvent,
    updatePoolConnectionAndReturnPoolConnectionUpdatedEvent,
} from "../gdav1.helper";
import { Pool } from "../../../generated/schema";
import { handlePoolConnectionUpdated } from "../../../src/mappings/gdav1";
import { mockedGetAppManifest, mockedRealtimeBalanceOf } from "../../mockedFunctions";

const initialFlowRate = BigInt.fromI32(100);
const superToken = maticXAddress;
const admin = alice;

describe("GeneralDistributionAgreementV1 Higher Order Level Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handlePoolCreated() - Should create a new Pool entity (create)", () => {
        const poolCreatedEvent = createPoolAndReturnPoolCreatedEvent(admin, superToken, superfluidPool);

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

    test("handlePoolConnectionUpdated() - Disconnected member connection updated: Pool entity is unchanged", () => {
        createPoolAndReturnPoolCreatedEvent(admin, superToken, superfluidPool);

        const account = bob;
        const connected = true;

        const poolConnectionUpdatedEvent = updatePoolConnectionAndReturnPoolConnectionUpdatedEvent(
            superToken,
            account,
            superfluidPool,
            connected,
            initialFlowRate
        );

        const id = poolConnectionUpdatedEvent.params.pool.toHexString();
        assertHigherOrderBaseProperties("Pool", id, poolConnectionUpdatedEvent);
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
        assert.fieldEquals("Pool", id, "token", poolConnectionUpdatedEvent.params.token.toHexString());
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
