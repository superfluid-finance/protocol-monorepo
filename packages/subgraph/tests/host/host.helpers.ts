import { Address, ethereum } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import { GovernanceReplaced } from "../../generated/Host/ISuperfluid";

export function createNewGovernanceReplacedEvent(
    oldGovernance: string,
    newGovernance: string
): GovernanceReplaced {
    let mockEvent = newMockEvent();
    let newGovernanceReplacedEvent = new GovernanceReplaced(
        mockEvent.address,
        mockEvent.logIndex,
        mockEvent.transactionLogIndex,
        mockEvent.logType,
        mockEvent.block,
        mockEvent.transaction,
        mockEvent.parameters,
        mockEvent.receipt
    );
    newGovernanceReplacedEvent.parameters = new Array();
    let oldGov = new ethereum.EventParam(
        "oldGov",
        ethereum.Value.fromAddress(Address.fromString(oldGovernance))
    );
    let newGov = new ethereum.EventParam(
        "newGov",
        ethereum.Value.fromAddress(Address.fromString(newGovernance))
    );
    newGovernanceReplacedEvent.parameters.push(oldGov);
    newGovernanceReplacedEvent.parameters.push(newGov);

    return newGovernanceReplacedEvent;
}
