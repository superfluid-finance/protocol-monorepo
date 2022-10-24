import { newMockEvent } from "matchstick-as";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import { FALSE, maticx, TRUE } from "../constants";
import {
    assertEventBaseProperties,
    assertHigherOrderBaseProperties,
} from "../assertionHelper";
import { createSetEvent } from "./resolver.helper";
import { handleSet } from "../../src/mappings/resolver";
import { stringToBytes } from "../converters";
import { createSuperToken } from "../helpers";
import { Address } from "@graphprotocol/graph-ts";

/**
 * Creates a Set Event and executes handleSet and asserts ResolverEntry fields based on params
 * @param tokenAddress the address of a token, if zero address, the ResolverEntry is not considered a token
 * @param target the target address
 * @returns ResolverEntry id
 */
function testResolverEntryParams(
    tokenAddress: Address,
    target: Address
): string {
    const name = stringToBytes("supertokens.v1.maticx");
    const setEvent = createSetEvent(name, target.toHexString());
    const isToken = tokenAddress.equals(Address.zero()) ? FALSE : TRUE;
    const isListed = target.equals(Address.zero()) ? FALSE : TRUE;

    handleSet(setEvent);

    const resolverEntryId = name.toHex();
    assertHigherOrderBaseProperties(
        "ResolverEntry",
        resolverEntryId,
        setEvent.block.timestamp,
        setEvent.block.number,
        setEvent.block.timestamp,
        setEvent.block.number
    );
    assert.fieldEquals(
        "ResolverEntry",
        resolverEntryId,
        "targetAddress",
        target.toHexString()
    );

    assert.fieldEquals("ResolverEntry", resolverEntryId, "isToken", isToken);
    assert.fieldEquals("ResolverEntry", resolverEntryId, "isListed", isListed);
    return resolverEntryId;
}

describe("Resolver Mapper Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    describe("Event Entity Mapping Tests", () => {
        test("handleSet() - Should create a new SetEvent entity", () => {
            const name = stringToBytes("supertokens.v1.maticx");
            const target = maticx;
            const setEvent = createSetEvent(name, target);

            handleSet(setEvent);

            const id = assertEventBaseProperties(setEvent, "Set");
            assert.fieldEquals(
                "SetEvent",
                id,
                "hashedName",
                name.toHexString()
            );
            assert.fieldEquals("SetEvent", id, "target", target);
            assert.fieldEquals(
                "SetEvent",
                id,
                "resolverEntry",
                name.toHexString()
            );
        });
    });

    describe("Higher Order Entity Mapping Tests", () => {
        test("Should create a ResolverEntry entity (no token case) - list case", () => {
            const token = Address.zero();
            const target = Address.fromString(maticx);
            testResolverEntryParams(token, target);
        });

        test("Should create a ResolverEntry entity (no token case) - delist case", () => {
            const token = Address.zero();
            let target = Address.fromString(maticx);
            testResolverEntryParams(token, target);
            target = Address.zero();
            testResolverEntryParams(token, target);
        });

        test("Should create a ResolverEntry entity (token case) - list case", () => {
            const mockEvent = newMockEvent();
            const token = Address.fromString(maticx);
            createSuperToken(
                token,
                mockEvent.block,
                18,
                "Super Matic",
                "MATICx",
                false,
                Address.zero()
            );
            const target = Address.fromString(maticx);
            assert.fieldEquals("Token", maticx, "isListed", FALSE);
            // list token on resolver
            testResolverEntryParams(token, target);
            assert.fieldEquals("Token", maticx, "isListed", TRUE);
        });

        test("Should create a ResolverEntry entity (token case) - delist case", () => {
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
            const token = Address.fromString(maticx);
            let target = Address.fromString(maticx);

            assert.fieldEquals("Token", maticx, "isListed", FALSE);

            // list token on resolver
            testResolverEntryParams(token, target);
            assert.fieldEquals("Token", maticx, "isListed", TRUE);

            // delist token from resolver
            target = Address.zero();
            testResolverEntryParams(token, target);
            assert.fieldEquals("Token", maticx, "isListed", FALSE);
        });
    });
});
