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
    createNewAgreementClassRegisteredEvent,
    createNewAppRegisteredEvent,
    createNewGovernanceReplacedEvent,
    createNewJailEvent,
    createNewSuperTokenFactoryUpdatedEvent,
    createNewSuperTokenLogicUpdatedEvent,
} from "./host.helper";
import { alice, bob } from "../constants";
import { eventBasePropertyAssertion } from "../assertionHelper";

describe("Host Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleGovernanceReplaced() - Should create a new GovernanceReplacedEvent entity", () => {
            const oldGov = alice;
            const newGov = bob;
            const governanceReplacedEvent = createNewGovernanceReplacedEvent(
                oldGov,
                newGov
            );

            handleGovernanceReplaced(governanceReplacedEvent);

            const id = eventBasePropertyAssertion(
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
                createNewAgreementClassRegisteredEvent(agreementType, code);

            handleAgreementClassRegistered(agreementClassRegisteredEvent);

            const id = eventBasePropertyAssertion(
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
            const SuperTokenFactoryUpdatedEvent =
                createNewSuperTokenFactoryUpdatedEvent(newFactory);

            handleSuperTokenFactoryUpdated(SuperTokenFactoryUpdatedEvent);

            const id = eventBasePropertyAssertion(
                SuperTokenFactoryUpdatedEvent,
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
            const SuperTokenLogicUpdatedEvent =
                createNewSuperTokenLogicUpdatedEvent(token, code);

            handleSuperTokenLogicUpdated(SuperTokenLogicUpdatedEvent);

            const id = eventBasePropertyAssertion(
                SuperTokenLogicUpdatedEvent,
                "SuperTokenLogicUpdated"
            );
            assert.fieldEquals("SuperTokenLogicUpdatedEvent", id, "code", code);
        });

        test("handleAppRegistered() - Should create a new AppRegisteredEvent entity", () => {
            const app = alice;
            const AppRegisteredEvent = createNewAppRegisteredEvent(app);

            handleAppRegistered(AppRegisteredEvent);

            const id = eventBasePropertyAssertion(
                AppRegisteredEvent,
                "AppRegistered"
            );
            assert.fieldEquals("AppRegisteredEvent", id, "app", app);
        });

        test("handleJail() - Should create a new JailEvent entity", () => {
            const app = alice;
            const reason = BigInt.fromI32(10); // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
            const JailEvent = createNewJailEvent(app, reason);

            handleJail(JailEvent);

            const id = eventBasePropertyAssertion(JailEvent, "Jail");
            assert.fieldEquals("JailEvent", id, "app", app);
            assert.fieldEquals("JailEvent", id, "reason", reason.toString());
        });
    });
});
