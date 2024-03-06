import { handleMemberUnitsUpdated } from "../../src/mappings/superfluidPool";
import { createMemberUnitsUpdatedEvent } from "../gdav1/gdav1.helper";
import { Address, BigInt } from "@graphprotocol/graph-ts";
import { FAKE_INITIAL_BALANCE, alice, bob, charlie } from "../constants";
import { BIG_INT_ZERO, getPoolMemberID } from "../../src/utils";
import { assert, describe, test } from "matchstick-as";
import { mockedGetAppManifest, mockedRealtimeBalanceOf } from "../mockedFunctions";

describe("PoolMember not updating when units changed", () => {
    test("emit MemberUnitsUpdated event", () => {
        const superTokenAddress = alice;

        const poolAddress = Address.fromString(bob);
        const poolMemberAccountAddress = Address.fromString(charlie);
        const poolMemberId = getPoolMemberID(poolAddress, poolMemberAccountAddress);

        const flowRate = BigInt.fromI32(69);
        const oldUnits = BigInt.fromI32(100);
        const newUnits = BigInt.fromI32(200);

        const memberUnitsUpdatedEvent = createMemberUnitsUpdatedEvent(
            superTokenAddress,
            poolMemberAccountAddress.toHexString(),
            oldUnits,
            newUnits
        );
        memberUnitsUpdatedEvent.block.number = BigInt.fromI32(300);
        memberUnitsUpdatedEvent.address = poolAddress;

        mockedGetAppManifest(poolMemberAccountAddress.toHexString(), false, false, BIG_INT_ZERO);

        mockedRealtimeBalanceOf(
            superTokenAddress,
            poolMemberAccountAddress.toHexString(),
            memberUnitsUpdatedEvent.block.timestamp,
            FAKE_INITIAL_BALANCE.minus(flowRate),
            flowRate,
            BIG_INT_ZERO
        );

        // Act
        handleMemberUnitsUpdated(memberUnitsUpdatedEvent);

        memberUnitsUpdatedEvent.block.number = BigInt.fromI32(420);

        handleMemberUnitsUpdated(memberUnitsUpdatedEvent);

        // // Assert
        assert.fieldEquals(
            "PoolMember",
            poolMemberId,
            "updatedAtBlockNumber",
            memberUnitsUpdatedEvent.block.number.toString()
        );
    });
});
