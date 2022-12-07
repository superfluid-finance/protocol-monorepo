import {
    BondIncreased,
    ExitRateChanged,
    NewPIC,
} from "../../generated/templates/TOGA/TOGA";
import {
    BondIncreasedEvent,
    ExitRateChangedEvent,
    NewPICEvent,
} from "../../generated/schema";
import { createEventID, initializeEventEntity } from "../utils";

export function handleNewPIC(event: NewPIC): void {
    const eventId = createEventID("NewPIC", event);
    const ev = new NewPICEvent(eventId);
    initializeEventEntity(ev, event, [event.params.token, event.params.pic]);

    ev.token = event.params.token;
    ev.pic = event.params.pic;
    ev.bond = event.params.bond;
    ev.exitRate = event.params.exitRate;

    ev.save();
}

export function handleExitRateChanged(event: ExitRateChanged): void {
    const eventId = createEventID("ExitRateChanged", event);
    const ev = new ExitRateChangedEvent(eventId);
    initializeEventEntity(ev, event, [event.params.token]);

    ev.token = event.params.token;
    ev.exitRate = event.params.exitRate;

    ev.save();
}

export function handleBondIncreased(event: BondIncreased): void {
    const eventId = createEventID("BondIncreased", event);
    const ev = new BondIncreasedEvent(eventId);
    initializeEventEntity(ev, event, [event.params.token]);

    ev.token = event.params.token;
    ev.additionalBond = event.params.additionalBond;

    ev.save();
}
