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
    const ev = new RoleAdminChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.role = event.params.role;
    ev.previousAdminRole = event.params.previousAdminRole;
    ev.newAdminRole = event.params.newAdminRole;
    ev.save();
}

export function handleRoleGranted(event: RoleGranted): void {
    const eventId = createEventID("RoleGranted", event);
    const ev = new RoleGrantedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.role = event.params.role;
    ev.account = event.params.account;
    ev.sender = event.params.sender;
    ev.save();
}
export function handleRoleRevoked(event: RoleRevoked): void {
    const eventId = createEventID("RoleRevoked", event);
    const ev = new RoleRevokedEvent(eventId);
    initializeEventEntity(ev, event, []);

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
    const ev = new SetEvent(eventId);
    initializeEventEntity(ev, event, [target]);

    ev.hashedName = name;
    ev.target = target;
    ev.resolverEntry = name.toHex();
    ev.save();
}
