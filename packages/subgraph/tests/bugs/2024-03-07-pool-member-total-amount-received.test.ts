import { assert, describe, log, test } from "matchstick-as";
import { Pool, PoolDistributor, PoolMember } from "../../generated/schema"
import { Address, BigInt, Bytes } from "@graphprotocol/graph-ts";
import { FAKE_INITIAL_BALANCE, alice as alice_, bob as bob_, charlie, delta, echo, maticXAddress, superfluidPool } from "../constants";
import { BIG_INT_ONE, BIG_INT_ZERO, getPoolMemberID } from "../../src/utils";
import { handleInstantDistributionUpdated } from "../../src/mappings/gdav1";
import { createInstantDistributionUpdatedEvent, createMemberUnitsUpdatedEvent } from "../gdav1/gdav1.helper";
import { mockedGetAppManifest, mockedRealtimeBalanceOf } from "../mockedFunctions";
import { handleMemberUnitsUpdated } from "../../src/mappings/superfluidPool";

/**
 * Problem description
    1. Create pool
    2. Add member A and update A units to 100
    3. Distribute 100 tokens
    4. Add member B and update B units to 100
    4. Distribute 100 tokens

    Expected result:
    member A 150 tokens
    member B 50 tokens

    Actual result:
    member A 100 tokens
    member B 50 tokens
 */
describe("PoolMember ending up with wrong `totalAmountReceivedUntilUpdatedAt`", () => {
    test("create elaborate scenario with 2 distributions and 2 pool members", () => {
        const superTokenAddress = maticXAddress;
        
        // # Arrange State 1
        // ## Arrange Pool
        const poolAddress = Address.fromString(superfluidPool);
        const poolAdminAndDistributorAddress = Address.fromString(delta);
        let pool = new Pool(poolAddress.toHexString());
        pool.createdAtTimestamp = BIG_INT_ONE;
        pool.createdAtBlockNumber = BIG_INT_ONE;
        pool.updatedAtTimestamp = BIG_INT_ONE;
        pool.updatedAtBlockNumber = BIG_INT_ONE;
        
        pool.totalMembers = 1;
        pool.totalConnectedMembers = 1;
        pool.totalDisconnectedMembers = 0;
        pool.adjustmentFlowRate = BIG_INT_ZERO;
        pool.flowRate = BIG_INT_ZERO;
        pool.admin = poolAdminAndDistributorAddress.toHexString();
        pool.totalBuffer = BIG_INT_ZERO;
        pool.token = superTokenAddress;
        pool.perUnitFlowRate = BIG_INT_ZERO;
        pool.perUnitSettledValue = BIG_INT_ZERO;
        pool.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalAmountFlowedDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalAmountInstantlyDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalFlowAdjustmentAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalConnectedUnits = BigInt.fromI32(100);
        pool.totalDisconnectedUnits = BIG_INT_ZERO;
        pool.totalUnits = BigInt.fromI32(100);
        pool.save();
        // ---

        // ## Arrange PoolMember 1
        const aliceAddress = Address.fromString(alice_);
        const aliceId = getPoolMemberID(poolAddress, aliceAddress);
        const alice = new PoolMember(aliceId)
        alice.createdAtTimestamp = BIG_INT_ONE;
        alice.createdAtBlockNumber = BIG_INT_ONE;
        alice.updatedAtTimestamp = BIG_INT_ONE;
        alice.updatedAtBlockNumber = BIG_INT_ONE;

        alice.account = aliceAddress.toHexString();
        alice.units = BigInt.fromI32(100);
        alice.totalAmountReceivedUntilUpdatedAt = BIG_INT_ZERO;
        alice.poolTotalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        alice.syncedPerUnitFlowRate = BIG_INT_ZERO;
        alice.syncedPerUnitSettledValue = BIG_INT_ZERO;
        alice.isConnected = true;
        alice.totalAmountClaimed = BIG_INT_ZERO;
        alice.pool = poolAddress.toHexString();
        alice.save();
        // # ---

        // ## Arrange Distributor
        const poolDistributor = new PoolDistributor(poolAdminAndDistributorAddress.toHexString());
        poolDistributor.createdAtTimestamp = BIG_INT_ONE;
        poolDistributor.createdAtBlockNumber = BIG_INT_ONE;
        poolDistributor.updatedAtTimestamp = BIG_INT_ONE;
        poolDistributor.updatedAtBlockNumber = BIG_INT_ONE;
        poolDistributor.account = charlie;
        poolDistributor.totalBuffer = BIG_INT_ZERO;
        poolDistributor.flowRate = BIG_INT_ZERO;
        poolDistributor.pool = poolAddress.toHexString();
        poolDistributor.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        poolDistributor.totalAmountFlowedDistributedUntilUpdatedAt = BIG_INT_ZERO;
        poolDistributor.totalAmountInstantlyDistributedUntilUpdatedAt = BIG_INT_ZERO;
        poolDistributor.save();
        // ---

        // # First distribution (State 2)
        const instantDistributionEvent = createInstantDistributionUpdatedEvent(
            superTokenAddress,
            poolAddress.toHexString(),
            poolAdminAndDistributorAddress.toHexString(),
            echo,
            BigInt.fromI32(100), // requested amount 
            BigInt.fromI32(100), // actual amount
            Bytes.fromHexString("0x")
        );
        instantDistributionEvent.block.timestamp = BIG_INT_ONE;
        instantDistributionEvent.address = poolAddress;
            
        mockedGetAppManifest(poolAdminAndDistributorAddress.toHexString(), false, false, BIG_INT_ZERO);
        mockedRealtimeBalanceOf(
            superTokenAddress,
            poolAdminAndDistributorAddress.toHexString(),
            BIG_INT_ONE,
            FAKE_INITIAL_BALANCE,
            BIG_INT_ZERO,
            BIG_INT_ZERO
        );
            
        handleInstantDistributionUpdated(instantDistributionEvent);

        assert.fieldEquals(
            "Pool",
            poolAddress.toHexString(),
            "totalAmountDistributedUntilUpdatedAt",
            "100"
        );
        assert.fieldEquals(
            "PoolMember",
            aliceId,
            "totalAmountReceivedUntilUpdatedAt",
            "0"
        );
        // # ---

        const bobAddress = Address.fromString(bob_);
        const bobId = getPoolMemberID(poolAddress, bobAddress);
        let updateBobUnitsEvent = createMemberUnitsUpdatedEvent(
            superTokenAddress,
            bobAddress.toHexString(),
            BigInt.fromI32(100), // old units
            BigInt.fromI32(100) // new units
        );

        updateBobUnitsEvent.address = poolAddress;
        updateBobUnitsEvent.block.timestamp = BigInt.fromI32(2);

        mockedGetAppManifest(bobAddress.toHexString(), false, false, BIG_INT_ZERO);
        mockedRealtimeBalanceOf(
            superTokenAddress,
            bobAddress.toHexString(),
            BigInt.fromI32(2),
            FAKE_INITIAL_BALANCE,
            BIG_INT_ZERO,
            BIG_INT_ZERO
        );

        handleMemberUnitsUpdated(updateBobUnitsEvent);
        // Note, the units can stay the same, we just want to trigger an update.

        // # Arrange State 3
        // ## Arrange PoolMember 2 (new member)
        // const bobId = getPoolMemberID(poolAddress, bobAddress);
        // const bob = new PoolMember(bobId)
        // bob.createdAtTimestamp = BIG_INT_ONE;
        // bob.createdAtBlockNumber = BIG_INT_ONE;
        // bob.updatedAtTimestamp = BIG_INT_ONE;
        // bob.updatedAtBlockNumber = BIG_INT_ONE;

        // bob.account = bobAddress.toHexString();
        // bob.units = BigInt.fromI32(100);
        // bob.totalAmountReceivedUntilUpdatedAt = BIG_INT_ZERO;
        // bob.poolTotalAmountDistributedUntilUpdatedAt = BigInt.fromI32(100);
        // bob.isConnected = true;
        // bob.totalAmountClaimed = BIG_INT_ZERO;
        // bob.pool = poolAddress.toHexString();

        // // get pool to use the synced per unit flow rate and settled value
        // pool = Pool.load(poolAddress.toHexString())!;
        // bob.syncedPerUnitFlowRate = pool.perUnitFlowRate;
        // bob.syncedPerUnitSettledValue = pool.perUnitSettledValue;
        
        // bob.save();
        // # ---

        // ## Update Pool for member 2
        pool = Pool.load(poolAddress.toHexString())!;
        pool.updatedAtTimestamp = BigInt.fromI32(2);
        pool.updatedAtBlockNumber = BigInt.fromI32(2);
        pool.totalMembers = 2;
        pool.totalConnectedMembers = 2;
        pool.totalDisconnectedMembers = 0;
        pool.totalConnectedUnits = BigInt.fromI32(200);
        pool.totalUnits = BigInt.fromI32(200);
        pool.save();
        // ---

        // # Second distribution (we can use the first event again) (State 4)
        handleInstantDistributionUpdated(instantDistributionEvent);
        
        assert.fieldEquals(
            "Pool",
            poolAddress.toHexString(),
            "totalAmountDistributedUntilUpdatedAt",
            "200"
        );
        assert.fieldEquals(
            "Pool",
            poolAddress.toHexString(),
            "totalUnits",
            "200"
        );
        // # ---

        // # Update PoolMember 2's units to get the `totalAmountReceivedUntilUpdatedAt`
        updateBobUnitsEvent = createMemberUnitsUpdatedEvent(
            superTokenAddress,
            bobAddress.toHexString(),
            BigInt.fromI32(100), // old units
            BigInt.fromI32(100) // new units
        );
        // Note, the units can stay the same, we just want to trigger an update.
        updateBobUnitsEvent.address = poolAddress;
        updateBobUnitsEvent.block.timestamp = BigInt.fromI32(2);

        mockedGetAppManifest(bobAddress.toHexString(), false, false, BIG_INT_ZERO);
        mockedRealtimeBalanceOf(
            superTokenAddress,
            bobAddress.toHexString(),
            BigInt.fromI32(2),
            FAKE_INITIAL_BALANCE,
            BIG_INT_ZERO,
            BIG_INT_ZERO
        );

        // Act 1
        log.debug("Act 1", []);
        handleMemberUnitsUpdated(updateBobUnitsEvent);
        
        assert.fieldEquals(
            "Pool",
            poolAddress.toHexString(),
            "totalAmountDistributedUntilUpdatedAt",
            "200"
        );
        assert.fieldEquals(
            "Pool",
            poolAddress.toHexString(),
            "totalUnits",
            "200"
        );
        assert.fieldEquals(
            "PoolMember",
            bobId,
            "totalAmountReceivedUntilUpdatedAt",
            "50"
        );

        // # Update PoolMember 1's units to get the `totalAmountReceivedUntilUpdatedAt`
        const updateAliceUnitsEvent = createMemberUnitsUpdatedEvent(
            superTokenAddress,
            aliceAddress.toHexString(),
            BigInt.fromI32(10), // old units
            BigInt.fromI32(10) // new units
        );
        // Note, the units can stay the same, we just want to trigger an update.
        updateAliceUnitsEvent.address = poolAddress;
        updateAliceUnitsEvent.block.timestamp = BigInt.fromI32(3);

        mockedGetAppManifest(aliceAddress.toHexString(), false, false, BIG_INT_ZERO);
        mockedRealtimeBalanceOf(
            superTokenAddress,
            aliceAddress.toHexString(),
            BigInt.fromI32(3),
            FAKE_INITIAL_BALANCE,
            BIG_INT_ZERO,
            BIG_INT_ZERO
        );

        // Act 2
        handleMemberUnitsUpdated(updateAliceUnitsEvent);

        assert.fieldEquals(
            "PoolMember",
            aliceId,
            "totalAmountReceivedUntilUpdatedAt",
            "150" // 100 from first + 50 from second
        );
    })
});
 