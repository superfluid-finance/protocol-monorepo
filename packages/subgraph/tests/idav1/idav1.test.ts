import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as";
import { handleIndexDistributionClaimed, handleIndexSubscribed, handleIndexUnitsUpdated, handleIndexUnsubscribed } from "../../src/mappings/idav1";
import { getIndexID } from "../../src/utils";
import { assertEventBaseProperties } from "../assertionHelper";
import { alice, bob, maticx } from "../constants";
import { stringToBytes } from "../converters";
import { createIndexDistributionClaimedEvent, createIndexSubscribedEvent, createIndexUnitsUpdatedEvent, createIndexUnsubscribedEvent } from "./idav1.helper";

describe("Instant Distribution V1 Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();

        //     // const mockEvent = newMockEvent();
        //     // createSuperToken(
        //     //     Address.fromString(maticx),
        //     //     mockEvent.block,
        //     //     18,
        //     //     "Super Matic",
        //     //     "MATICx",
        //     //     false,
        //     //     Address.zero()
        //     // );

        //     // // @note we must create a mocked function for
        //     // // the resolver contract, otherwise we get the
        //     // // "Could not find a mocked function" error
        //     createMockedFunction(
        //         Address.fromString(maticx),
        //         "getHost",
        //         "getHost():(address)"
        //     )
        //         .withArgs([])
        //         .returns([
        //             ethereum.Value.fromAddress(Address.fromString(hostAddress)),
        //         ]);
        //     const tupleArray: Array<ethereum.Value> = [
        //         ethereum.Value.fromBoolean(false),
        //         ethereum.Value.fromBoolean(false),
        //         ethereum.Value.fromUnsignedBigInt(BigInt.fromI32(0)),
        //     ];
        //     const tuple = new ethereum.Tuple(1);
        //     createMockedFunction(
        //         Address.fromString(hostAddress),
        //         "getAppManifest",
        //         "getAppManifest():(address)"
        //     )
        //         .withArgs([])
        //         .returns([ethereum.Value.fromTuple(tuple)]);

        //     // createMockedFunction(
        //     //     Address.fromString(resolverAddress),
        //     //     "get",
        //     //     "get(string):(address)"
        //     // )
        //     //     .withArgs([
        //     //         ethereum.Value.fromString("supertokens.test.MATICx"),
        //     //     ])
        //     //     .returns([
        //     //         ethereum.Value.fromAddress(Address.fromString(maticx)),
        //     //     ]);
        });

        // test("handleIndexCreated() - Should create a new IndexCreatedEvent entity", () => {
        //     const token = maticx;
        //     const publisher = alice;
        //     const indexId = BigInt.fromI32(1);
        //     const userData = stringToBytes("");

        //     const indexCreatedEvent = createIndexCreatedEvent(
        //         token,
        //         publisher,
        //         indexId,
        //         userData
        //     );

        //     handleIndexCreated(indexCreatedEvent);

        //     const id = assertEventBaseProperties(
        //         indexCreatedEvent,
        //         "IndexCreated"
        //     );
        //     const indexEntityId = getIndexID(
        //         Address.fromString(publisher),
        //         Address.fromString(token),
        //         indexId
        //     );

        //     assert.fieldEquals("IndexCreatedEvent", id, "index", indexEntityId);
        // });

        test("handleIndexDistributionClaimed() - Should create a new IndexDistributionClaimedEvent entity", () => {
            const token = maticx;
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
            const token = maticx;
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
            const token = maticx;
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
