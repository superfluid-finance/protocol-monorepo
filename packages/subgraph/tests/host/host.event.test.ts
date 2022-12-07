import { BigInt, ByteArray, Bytes, crypto } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import {
    handleAgreementClassRegistered,
    handleAppRegistered,
    handleGovernanceReplaced,
    handleJail,
    handleSuperTokenFactoryUpdated,
    handleSuperTokenLogicUpdated,
} from "../../src/mappings/host";
import {
    createAgreementClassRegisteredEvent,
    createAppRegisteredEvent,
    createGovernanceReplacedEvent,
    createJailEvent,
    createSuperTokenFactoryUpdatedEvent,
    createSuperTokenLogicUpdatedEvent,
} from "./host.helper";
import { alice, bob } from "../constants";
import { assertEventBaseProperties } from "../assertionHelpers";

describe("Host Event Entity Unit Tests", () => {
    beforeEach(() => {
        clearStore();
    });

    test("handleGovernanceReplaced() - Should create a new GovernanceReplacedEvent entity", () => {
        const oldGov = alice;
        const newGov = bob;
        const governanceReplacedEvent = createGovernanceReplacedEvent(
            oldGov,
            newGov
        );

        handleGovernanceReplaced(governanceReplacedEvent);

        const id = assertEventBaseProperties(
            governanceReplacedEvent,
            "GovernanceReplaced"
        );
        assert.fieldEquals(
            "GovernanceReplacedEvent",
            id,
            "oldGovernance",
            oldGov
        );
        assert.fieldEquals(
            "GovernanceReplacedEvent",
            id,
            "newGovernance",
            newGov
        );
    });

    test("handleAgreementClassRegistered() - Should create a new AgreementClassRegisteredEvent entity", () => {
        const code = alice;
        const bytesString = ByteArray.fromUTF8(
            "org.superfluid-finance.contracts.Superfluid.implementation"
        );
        const agreementType = Bytes.fromByteArray(
            crypto.keccak256(bytesString)
        );
        const agreementClassRegisteredEvent =
            createAgreementClassRegisteredEvent(agreementType, code);

        handleAgreementClassRegistered(agreementClassRegisteredEvent);

        const id = assertEventBaseProperties(
            agreementClassRegisteredEvent,
            "AgreementClassRegistered"
        );
        assert.fieldEquals(
            "AgreementClassRegisteredEvent",
            id,
            "agreementType",
            agreementType.toHexString()
        );
        assert.fieldEquals(
            "AgreementClassRegisteredEvent",
            id,
            "code",
            code
        );
    });

    test("handleSuperTokenFactoryUpdated() - Should create a new SuperTokenFactoryUpdatedEvent entity", () => {
        const newFactory = alice;
        const superTokenFactoryUpdatedEvent =
            createSuperTokenFactoryUpdatedEvent(newFactory);

        handleSuperTokenFactoryUpdated(superTokenFactoryUpdatedEvent);

        const id = assertEventBaseProperties(
            superTokenFactoryUpdatedEvent,
            "SuperTokenFactoryUpdated"
        );
        assert.fieldEquals(
            "SuperTokenFactoryUpdatedEvent",
            id,
            "newFactory",
            newFactory
        );
    });

    test("handleSuperTokenLogicUpdated() - Should create a new SuperTokenLogicUpdatedEvent entity", () => {
        const token = alice;
        const code = token;
        const superTokenLogicUpdatedEvent =
            createSuperTokenLogicUpdatedEvent(token, code);

        handleSuperTokenLogicUpdated(superTokenLogicUpdatedEvent);

        const id = assertEventBaseProperties(
            superTokenLogicUpdatedEvent,
            "SuperTokenLogicUpdated"
        );
        assert.fieldEquals("SuperTokenLogicUpdatedEvent", id, "code", code);
    });

    test("handleAppRegistered() - Should create a new AppRegisteredEvent entity", () => {
        const app = alice;
        const appRegisteredEvent = createAppRegisteredEvent(app);

        handleAppRegistered(appRegisteredEvent);

        const id = assertEventBaseProperties(
            appRegisteredEvent,
            "AppRegistered"
        );
        assert.fieldEquals("AppRegisteredEvent", id, "app", app);
    });

    test("handleJail() - Should create a new JailEvent entity", () => {
        const app = alice;
        const reason = BigInt.fromI32(10); // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
        const JailEvent = createJailEvent(app, reason);

        handleJail(JailEvent);

        const id = assertEventBaseProperties(JailEvent, "Jail");
        assert.fieldEquals("JailEvent", id, "app", app);
        assert.fieldEquals("JailEvent", id, "reason", reason.toString());
    });
});
