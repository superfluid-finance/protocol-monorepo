import { BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import { handleBurned, handleMinted, handleSent } from "../../src/mappings/superToken";
import { BIG_INT_ZERO } from "../../src/utils";
import { assertEventBaseProperties, assertTokenStatisticProperties } from "../assertionHelpers";
import {
    alice,
    bob
} from "../constants";
import { stringToBytes } from "../converters";
import { mockedGetHost } from "../mockedFunctions";
import { createBurnedEvent, createMintedEvent, createSentEvent } from "./superToken.helper";

describe("SuperToken Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleSent() - Should create a new SentEvent entity", () => {
            const operator = alice;
            const from = alice;
            const to = bob;
            const amount = BigInt.fromI32(100);
            const data = stringToBytes("");
            const operatorData = stringToBytes("");

            const sentEvent = createSentEvent(
                operator,
                from,
                to,
                amount,
                data,
                operatorData
            );

            mockedGetHost(sentEvent.address.toHex());

            handleSent(sentEvent);

            const id = assertEventBaseProperties(
                sentEvent,
                "Sent"
            );
            assert.fieldEquals("SentEvent", id, "operator", operator);
            assert.fieldEquals("SentEvent", id, "from", from);
            assert.fieldEquals("SentEvent", id, "to", to);
            assert.fieldEquals("SentEvent", id, "amount", amount.toString());
            assert.fieldEquals("SentEvent", id, "data", data.toHexString());
            assert.fieldEquals("SentEvent", id, "operatorData", operatorData.toHexString());
        });

        test("handleBurned() - Should create a new BurnedEvent entity", () => {
            const operator = alice;
            const from = bob;
            const amount = BigInt.fromI32(100);
            const data = stringToBytes("");
            const operatorData = stringToBytes("");

            const burnedEvent = createBurnedEvent(
                operator,
                from,
                amount,
                data, 
                operatorData
            );

            handleBurned(burnedEvent);

            const id = assertEventBaseProperties(
                burnedEvent,
                "Burned"
            );
            assert.fieldEquals("BurnedEvent", id, "operator", operator);
            assert.fieldEquals("BurnedEvent", id, "from", from);
            assert.fieldEquals("BurnedEvent", id, "amount", amount.toString());
            assert.fieldEquals("BurnedEvent", id, "data", data.toHexString());
            assert.fieldEquals("BurnedEvent", id, "operatorData", operatorData.toHexString());
        });

        test("handleMinted() - Should create a new MintedEvent entity", () => {
            const operator = alice;
            const to = bob;
            const amount = BigInt.fromI32(100);
            const data = stringToBytes("");
            const operatorData = stringToBytes("");

            const mintedEvent = createMintedEvent(
                operator,
                to,
                amount,
                data, 
                operatorData
            );

            handleMinted(mintedEvent);

            const id = assertEventBaseProperties(
                mintedEvent,
                "Minted"
            );
            assert.fieldEquals("MintedEvent", id, "operator", operator);
            assert.fieldEquals("MintedEvent", id, "to", to);
            assert.fieldEquals("MintedEvent", id, "amount", amount.toString());
            assert.fieldEquals("MintedEvent", id, "data", data.toHexString());
            assert.fieldEquals("MintedEvent", id, "operatorData", operatorData.toHexString());
        });
    });

    describe("Higher Order Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleBurned() - Should create a new TokenStatistic entity", () => {
            const operator = alice;
            const from = bob;
            const amount = BigInt.fromI32(100);
            const data = stringToBytes("");
            const operatorData = stringToBytes("");

            const burnedEvent = createBurnedEvent(
                operator,
                from,
                amount,
                data, 
                operatorData
            );

            handleBurned(burnedEvent);
            assertTokenStatisticProperties(
                null,
                null,
                burnedEvent.address.toHex(),
                burnedEvent.block.timestamp,
                burnedEvent.block.number,
                0,            // totalNumberOfActiveStreams
                0,            // totalNumberOfClosedStreams
                0,            // totalNumberOfIndexes
                0,            // totalNumberOfActiveIndexes
                0,            // totalSubscriptionsWithUnits
                0,            // totalApprovedSubscriptions
                BIG_INT_ZERO, // totalDeposit
                BIG_INT_ZERO, // totalOutflowRate
                BIG_INT_ZERO, // totalAmountStreamedUntilUpdatedAt
                BIG_INT_ZERO, // totalAmountTransferredUntilUpdatedAt
                BIG_INT_ZERO, // totalAmountDistributedUntilUpdatedAt
                amount.neg()  // totalSupply = -100 (not possible in practice)
            );
        });

        test("handleMinted() - Should create a new TokenStatistic entity", () => {
            const operator = alice;
            const to = bob;
            const amount = BigInt.fromI32(100);
            const data = stringToBytes("");
            const operatorData = stringToBytes("");

            const mintedEvent = createMintedEvent(
                operator,
                to,
                amount,
                data, 
                operatorData
            );

            handleMinted(mintedEvent);
            assertTokenStatisticProperties(
                null,
                null,
                mintedEvent.address.toHex(),
                mintedEvent.block.timestamp,
                mintedEvent.block.number,
                0,            // totalNumberOfActiveStreams
                0,            // totalNumberOfClosedStreams
                0,            // totalNumberOfIndexes
                0,            // totalNumberOfActiveIndexes
                0,            // totalSubscriptionsWithUnits
                0,            // totalApprovedSubscriptions
                BIG_INT_ZERO, // totalDeposit
                BIG_INT_ZERO, // totalOutflowRate
                BIG_INT_ZERO, // totalAmountStreamedUntilUpdatedAt
                BIG_INT_ZERO, // totalAmountTransferredUntilUpdatedAt
                BIG_INT_ZERO, // totalAmountDistributedUntilUpdatedAt
                amount        // totalSupply = 100
            );
        });
    });
});
