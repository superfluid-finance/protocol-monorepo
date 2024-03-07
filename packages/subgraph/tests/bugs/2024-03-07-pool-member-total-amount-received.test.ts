import { assert, describe, test } from "matchstick-as";
import { Pool, PoolDistributor, PoolMember } from "../../generated/schema"
import { Address, BigInt, Bytes } from "@graphprotocol/graph-ts";
import { FAKE_INITIAL_BALANCE, alice as alice_, bob as bob_, charlie, delta, echo, maticXAddress, superfluidPool } from "../constants";
import { BIG_INT_ZERO, getPoolMemberID } from "../../src/utils";
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
describe("PoolMember not updating when units changed", () => {
    test("emit MemberUnitsUpdated event", () => {
        const superTokenAddress = maticXAddress;
        
        // # Arrange State 1
        // ## Arrange Pool
        const poolAddress = Address.fromString(superfluidPool);
        const poolAdminAndDistributorAddress = Address.fromString(delta);
        let pool = new Pool(poolAddress.toHexString());
        pool.createdAtTimestamp = BigInt.fromI32(1);
        pool.createdAtBlockNumber = BigInt.fromI32(1);
        pool.updatedAtTimestamp = BigInt.fromI32(1);
        pool.updatedAtBlockNumber = BigInt.fromI32(1);
        
        pool.totalMembers = 1;
        pool.totalConnectedMembers = 1;
        pool.totalDisconnectedMembers = 0;
        pool.adjustmentFlowRate = BigInt.fromI32(0);
        pool.flowRate = BigInt.fromI32(0);
        pool.admin = poolAdminAndDistributorAddress.toHexString();
        pool.totalBuffer = BigInt.fromI32(0);
        pool.token = superTokenAddress;
        pool.totalAmountDistributedUntilUpdatedAt = BigInt.fromI32(0);
        pool.totalAmountFlowedDistributedUntilUpdatedAt = BigInt.fromI32(0);
        pool.totalAmountInstantlyDistributedUntilUpdatedAt = BigInt.fromI32(0);
        pool.totalConnectedUnits = BigInt.fromI32(100);
        pool.totalDisconnectedUnits = BigInt.fromI32(0);
        pool.totalUnits = BigInt.fromI32(100);
        pool.save();
        // ---

        // ## Arrange PoolMember 1
        const aliceAddress = Address.fromString(alice_);
        const aliceId = getPoolMemberID(poolAddress, aliceAddress);
        const alice = new PoolMember(aliceId)
        alice.createdAtTimestamp = BigInt.fromI32(1);
        alice.createdAtBlockNumber = BigInt.fromI32(1);
        alice.updatedAtTimestamp = BigInt.fromI32(1);
        alice.updatedAtBlockNumber = BigInt.fromI32(1);

        alice.account = aliceAddress.toHexString();
        alice.units = BigInt.fromI32(100);
        alice.totalAmountReceivedUntilUpdatedAt = BigInt.fromI32(0);
        alice.poolTotalAmountDistributedUntilUpdatedAt = BigInt.fromI32(0);
        alice.isConnected = true;
        alice.totalAmountClaimed = BigInt.fromI32(0);
        alice.pool = poolAddress.toHexString();
        alice.save();
        // # ---

        // ## Arrange Distributor
        const poolDistributor = new PoolDistributor(poolAdminAndDistributorAddress.toHexString());
        poolDistributor.createdAtTimestamp = BigInt.fromI32(1);
        poolDistributor.createdAtBlockNumber = BigInt.fromI32(1);
        poolDistributor.updatedAtTimestamp = BigInt.fromI32(1);
        poolDistributor.updatedAtBlockNumber = BigInt.fromI32(1);
        poolDistributor.account = charlie;
        poolDistributor.totalBuffer = BigInt.fromI32(0);
        poolDistributor.flowRate = BigInt.fromI32(0);
        poolDistributor.pool = poolAddress.toHexString();
        poolDistributor.totalAmountDistributedUntilUpdatedAt = BigInt.fromI32(0);
        poolDistributor.totalAmountFlowedDistributedUntilUpdatedAt = BigInt.fromI32(0);
        poolDistributor.totalAmountInstantlyDistributedUntilUpdatedAt = BigInt.fromI32(0);
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
        instantDistributionEvent.block.timestamp = BigInt.fromI32(1);
        instantDistributionEvent.address = poolAddress;
            
        mockedGetAppManifest(poolAdminAndDistributorAddress.toHexString(), false, false, BIG_INT_ZERO);
        mockedRealtimeBalanceOf(
            superTokenAddress,
            poolAdminAndDistributorAddress.toHexString(),
            BigInt.fromI32(1),
            FAKE_INITIAL_BALANCE,
            BigInt.fromI32(0),
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

        // # Arrange State 3
        // ## Arrange PoolMember 2 (new member)
        const bobAddress = Address.fromString(bob_);
        const bobId = getPoolMemberID(poolAddress, bobAddress);
        const bob = new PoolMember(bobId)
        bob.createdAtTimestamp = BigInt.fromI32(1);
        bob.createdAtBlockNumber = BigInt.fromI32(1);
        bob.updatedAtTimestamp = BigInt.fromI32(1);
        bob.updatedAtBlockNumber = BigInt.fromI32(1);

        bob.account = bobAddress.toHexString();
        bob.units = BigInt.fromI32(100);
        bob.totalAmountReceivedUntilUpdatedAt = BigInt.fromI32(0);
        bob.poolTotalAmountDistributedUntilUpdatedAt = BigInt.fromI32(100);
        bob.isConnected = true;
        bob.totalAmountClaimed = BigInt.fromI32(0);
        bob.pool = poolAddress.toHexString();
        bob.save();
        // # ---

        // ## Update Pool for member 2
        pool = Pool.load(poolAddress.toHexString())!;
        pool.updatedAtTimestamp = BigInt.fromI32(2);
        pool.updatedAtBlockNumber = BigInt.fromI32(2);
        pool.totalMembers = 2;
        pool.totalConnectedMembers = 2;
        pool.totalDisconnectedMembers = 0;
        pool.totalConnectedUnits = BigInt.fromI32(2000);
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
        const updateBobUnitsEvent = createMemberUnitsUpdatedEvent(
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
            BigInt.fromI32(0),
            BIG_INT_ZERO
        );

        // Act 1
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
            BigInt.fromI32(0),
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
 