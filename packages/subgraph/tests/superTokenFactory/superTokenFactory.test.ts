import { newMockEvent } from "matchstick-as";
import { Address } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "../../src/mappings/superTokenFactory";
import { assertEventBaseProperties } from "../assertionHelper";
import { maticx, resolverAddress, superTokenLogicAddress } from "../constants";
import { createSuperToken } from "../helpers";
import {
    createCustomSuperTokenCreatedEvent,
    createSuperTokenCreatedEvent,
    createSuperTokenLogicCreatedEvent,
} from "./superTokenFactory.helper";
import { mockedGetHost, mockedResolverGet } from "../mockedFunctions";

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
            mockedGetHost(maticx);
            mockedResolverGet(
                resolverAddress,
                "supertokens.test.MATICx",
                maticx
            );
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

            mockedGetHost(superTokenLogicAddress);

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
