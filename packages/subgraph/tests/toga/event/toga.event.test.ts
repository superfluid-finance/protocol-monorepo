import { BigInt } from "@graphprotocol/graph-ts";
import { assert, beforeEach, clearStore, describe, test } from "matchstick-as";
import { handleBondIncreased, handleExitRateChanged, handleNewPIC } from "../../../src/mappings/toga";
import { assertEventBaseProperties } from "../../assertionHelpers";
import { alice, maticXAddress } from "../../constants";
import {
    createBondIncreasedEvent,
    createExitRateChangedEvent,
    createNewPICEvent,
} from "../toga.helper";

const token = maticXAddress;
const pic = alice;
const bond = BigInt.fromI32(420);
const exitRate = BigInt.fromI32(69);
const additionalBond = BigInt.fromI32(42);


describe("TOGA Event Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handleNewPIC() - Should create a new NewPICEvent entity", () => {
        const newPICEvent = createNewPICEvent(token, pic, bond, exitRate);

        handleNewPIC(newPICEvent);

        const id = assertEventBaseProperties(newPICEvent, "NewPIC");
        assert.fieldEquals("NewPICEvent", id, "token", token);
        assert.fieldEquals("NewPICEvent", id, "pic", pic);
        assert.fieldEquals("NewPICEvent", id, "bond", bond.toString());
        assert.fieldEquals("NewPICEvent", id, "exitRate", exitRate.toString());
    });

    test("handleExitRateChanged() - Should create a new ExitRateChangedEvent entity", () => {
        const exitRateChangedEvent = createExitRateChangedEvent(token, exitRate);

        handleExitRateChanged(exitRateChangedEvent);

        const id = assertEventBaseProperties(exitRateChangedEvent, "ExitRateChanged");
        assert.fieldEquals("ExitRateChangedEvent", id, "token", token);
        assert.fieldEquals("ExitRateChangedEvent", id, "exitRate", exitRate.toString());
    });

    test("handleBondIncreased() - Should create a new BondIncreasedEvent entity", () => {
        const bondIncreasedEvent = createBondIncreasedEvent(token, additionalBond);
        
        handleBondIncreased(bondIncreasedEvent);

        const id = assertEventBaseProperties(bondIncreasedEvent, "BondIncreased");
        assert.fieldEquals("BondIncreasedEvent", id, "token", token);
        assert.fieldEquals("BondIncreasedEvent", id, "additionalBond", additionalBond.toString());
    });
});
