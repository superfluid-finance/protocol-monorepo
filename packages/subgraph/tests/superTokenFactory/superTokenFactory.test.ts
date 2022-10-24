+import { newMockEvent } from "matchstick-as";
import { Address, ethereum } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    createMockedFunction,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "../../src/mappings/superTokenFactory";
import { assertEventBaseProperties } from "../assertionHelper";
import {
    hostAddress,
    maticx,
    resolverAddress,
    superTokenLogicAddress,
} from "../constants";
import { createSuperToken } from "../helpers";
import {
    createCustomSuperTokenCreatedEvent,
    createSuperTokenCreatedEvent,
    createSuperTokenLogicCreatedEvent,
} from "./superTokenFactory.helper";

describe("SuperTokenFactory Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();

            const mockEvent = newMockEvent();
            createSuperToken(
                Address.fromString(maticx),
                mockEvent.block,
                18,
                "Super Matic",
                "MATICx",
                false,
                Address.zero()
            );

            // @note we must create a mocked function for
            // the resolver contract, otherwise we get the
            // "Could not find a mocked function" error
            createMockedFunction(
                Address.fromString(maticx),
                "getHost",
                "getHost():(address)"
            )
                .withArgs([])
                .returns([
                    ethereum.Value.fromAddress(Address.fromString(hostAddress)),
                ]);
            createMockedFunction(
                Address.fromString(resolverAddress),
                "get",
                "get(string):(address)"
            )
                .withArgs([
                    ethereum.Value.fromString("supertokens.test.MATICx"),
                ])
                .returns([
                    ethereum.Value.fromAddress(Address.fromString(maticx)),
                ]);
        });

        test("handleSuperTokenCreated() - Should create a new SuperTokenCreatedEvent entity", () => {
            const token = maticx;

            const SuperTokenCreatedEvent = createSuperTokenCreatedEvent(token);

            handleSuperTokenCreated(SuperTokenCreatedEvent);

            const id = assertEventBaseProperties(
                SuperTokenCreatedEvent,
                "SuperTokenCreated"
            );
            assert.fieldEquals("SuperTokenCreatedEvent", id, "token", token);
        });

        test("handleCustomSuperTokenCreated() - Should create a new CustomSuperTokenCreatedEvent entity", () => {
            const token = maticx;

            const CustomSuperTokenCreatedEvent =
                createCustomSuperTokenCreatedEvent(token);

            handleCustomSuperTokenCreated(CustomSuperTokenCreatedEvent);

            const id = assertEventBaseProperties(
                CustomSuperTokenCreatedEvent,
                "CustomSuperTokenCreated"
            );
            assert.fieldEquals(
                "CustomSuperTokenCreatedEvent",
                id,
                "token",
                token
            );
        });

        test("handleSuperTokenLogicCreated() - Should create a new SuperTokenLogicCreatedEvent entity", () => {
            const tokenLogic = superTokenLogicAddress;

            const SuperTokenLogicCreatedEvent =
                createSuperTokenLogicCreatedEvent(tokenLogic);

            createMockedFunction(
                Address.fromString(superTokenLogicAddress),
                "getHost",
                "getHost():(address)"
            )
                .withArgs([])
                .returns([
                    ethereum.Value.fromAddress(Address.fromString(hostAddress)),
                ]);

            handleSuperTokenLogicCreated(SuperTokenLogicCreatedEvent);

            const id = assertEventBaseProperties(
                SuperTokenLogicCreatedEvent,
                "SuperTokenLogicCreated"
            );
            assert.fieldEquals(
                "SuperTokenLogicCreatedEvent",
                id,
                "tokenLogic",
                tokenLogic
            );
        });
    });
});