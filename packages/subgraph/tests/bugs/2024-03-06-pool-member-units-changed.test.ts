import { handleMemberUnitsUpdated } from "../../src/mappings/superfluidPool";
import { createMemberUnitsUpdatedEvent } from "../gdav1/gdav1.helper";
import { Address, BigInt } from "@graphprotocol/graph-ts";
import { alice, bob, charlie } from "../constants";
import { getPoolMemberID } from "../../src/utils";
import { assert, describe, test } from "matchstick-as";

describe("PoolMember not updating when units changed", () => {
    test("emit MemberUnitsUpdated event", () => {
        const superTokenAddress = alice;

        const poolAddress = Address.fromString(bob);
        const poolMemberAccountAddress = Address.fromString(charlie);
        const poolMemberId = getPoolMemberID(poolAddress, poolMemberAccountAddress);

        const oldUnits = BigInt.fromI32(100);
        const newUnits = BigInt.fromI32(200);

        const memberUnitsUpdatedEvent = createMemberUnitsUpdatedEvent(
            superTokenAddress,
            poolMemberId,
            oldUnits,
            newUnits
        );
        memberUnitsUpdatedEvent.block.number = BigInt.fromI32(300);

        // // Act
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
