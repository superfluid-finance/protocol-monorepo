import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as";
import { handleIndexDistributionClaimed, handleIndexSubscribed, handleIndexUnsubscribed } from "../../src/mappings/idav1";
import { getIndexID } from "../../src/utils";
import { assertEventBaseProperties } from "../assertionHelpers";
import { alice, bob, maticXAddress } from "../constants";
import { stringToBytes } from "../converters";
import { createIndexDistributionClaimedEvent, createIndexSubscribedEvent, createIndexUnsubscribedEvent } from "./idav1.helper";

describe("Instant Distribution V1 Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();

        });

        test("handleIndexDistributionClaimed() - Should create a new IndexDistributionClaimedEvent entity", () => {
            const token = maticXAddress;
            const publisher = alice;
            const subscriber = bob;
            const indexId = BigInt.fromI32(1);
            const amount = BigInt.fromI32(100);

            const indexDistributionClaimedEvent = createIndexDistributionClaimedEvent(
                token,
                publisher,
                indexId,
                subscriber,
                amount
            );

            handleIndexDistributionClaimed(indexDistributionClaimedEvent);

            const id = assertEventBaseProperties(
                indexDistributionClaimedEvent,
                "IndexDistributionClaimed"
            );
            const indexEntityId = getIndexID(
                Address.fromString(publisher),
                Address.fromString(token),
                indexId
            );

            assert.fieldEquals("IndexDistributionClaimedEvent", id, "index", indexEntityId);
        });
        
        test("handleIndexSubscribed() - Should create a new IndexSubscribedEvent entity", () => {
            const token = maticXAddress;
            const publisher = alice;
            const subscriber = bob;
            const indexId = BigInt.fromI32(1);
            const userData = stringToBytes("");

            const indexSubscribedEvent = createIndexSubscribedEvent(
                token,
                publisher,
                indexId,
                subscriber,
                userData
            );

            handleIndexSubscribed(indexSubscribedEvent);

            const id = assertEventBaseProperties(
                indexSubscribedEvent,
                "IndexSubscribed"
            );
            const indexEntityId = getIndexID(
                Address.fromString(publisher),
                Address.fromString(token),
                indexId
            );

            assert.fieldEquals("IndexSubscribedEvent", id, "index", indexEntityId);
        });

        test("handleIndexUnsubscribed() - Should create a new IndexUnsubscribedEvent entity", () => {
            const token = maticXAddress;
            const publisher = alice;
            const subscriber = bob;
            const indexId = BigInt.fromI32(1);
            const userData = stringToBytes("");

            const indexUnsubscribedEvent = createIndexUnsubscribedEvent(
                token,
                publisher,
                indexId,
                subscriber,
                userData
            );

            handleIndexUnsubscribed(indexUnsubscribedEvent);

            const id = assertEventBaseProperties(
                indexUnsubscribedEvent,
                "IndexUnsubscribed"
            );
            const indexEntityId = getIndexID(
                Address.fromString(publisher),
                Address.fromString(token),
                indexId
            );

            assert.fieldEquals("IndexUnsubscribedEvent", id, "index", indexEntityId);
        });
    });
});
