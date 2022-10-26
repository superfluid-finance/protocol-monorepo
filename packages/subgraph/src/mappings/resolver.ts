import { Bytes, ethereum } from "@graphprotocol/graph-ts";
import {
    RoleAdminChanged,
    RoleGranted,
    RoleRevoked,
    Set,
} from "../../generated/ResolverV1/Resolver";
import {
    RoleAdminChangedEvent,
    RoleGrantedEvent,
    RoleRevokedEvent,
    SetEvent,
    Token,
} from "../../generated/schema";
import { getOrInitResolverEntry } from "../mappingHelpers";
import { createEventID, initializeEventEntity, ZERO_ADDRESS } from "../utils";

export function handleRoleAdminChanged(event: RoleAdminChanged): void {
    const eventId = createEventID("RoleAdminChanged", event);
    let ev = new RoleAdminChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "RoleAdminChanged",
        event,
        []
    ) as RoleAdminChangedEvent;

    ev.role = event.params.role;
    ev.previousAdminRole = event.params.previousAdminRole;
    ev.newAdminRole = event.params.newAdminRole;
    ev.save();
}

export function handleRoleGranted(event: RoleGranted): void {
    const eventId = createEventID("RoleGranted", event);
    let ev = new RoleGrantedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "RoleGranted",
        event,
        []
    ) as RoleGrantedEvent;

    ev.role = event.params.role;
    ev.account = event.params.account;
    ev.sender = event.params.sender;
    ev.save();
}
export function handleRoleRevoked(event: RoleRevoked): void {
    const eventId = createEventID("RoleRevoked", event);
    let ev = new RoleRevokedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "RoleRevoked",
        event,
        []
    ) as RoleRevokedEvent;

    ev.role = event.params.role;
    ev.account = event.params.account;
    ev.sender = event.params.sender;
    ev.save();
}

export function handleSet(event: Set): void {
    _createSetEvent(event, event.params.target, event.params.name);

    const resolverEntry = getOrInitResolverEntry(
        event.params.name.toHex(),
        event.params.target,
        event.block
    );

    if (resolverEntry.isToken) {
        const token = Token.load(resolverEntry.targetAddress.toHex());
        if (token) {
            if (event.params.target.equals(ZERO_ADDRESS)) {
                token.isListed = false;
            } else {
                token.isListed = true;
            }
            token.save();
        }
    }
}

function _createSetEvent(
    event: ethereum.Event,
    target: Bytes,
    name: Bytes
): void {
    const eventId = createEventID("Set", event);
    let ev = new SetEvent(eventId);
    ev = initializeEventEntity(ev, "Set", event, [target]) as SetEvent;

    ev.hashedName = name;
    ev.target = target;
    ev.resolverEntry = name.toHex();
    ev.save();
}
