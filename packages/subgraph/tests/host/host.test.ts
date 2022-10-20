import { describe, test, assert } from "matchstick-as/assembly/index";
import { handleGovernanceReplaced } from "../../src/mappings/host";
import { createNewGovernanceReplacedEvent } from "./host.helpers";
import { alice, bob } from "../constants";
import { eventBasePropertyAssertion } from "../assertionHelper";

describe("Host Mapping Unit Tests", () => {
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
});
