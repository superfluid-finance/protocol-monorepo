import {
    RoleAdminChanged,
    RoleGranted,
    RoleRevoked,
} from "../../generated/ResolverV1/Resolver";
import {
    RoleAdminChangedEvent,
    RoleGrantedEvent,
    RoleRevokedEvent,
} from "../../generated/schema";
import {createEventID, getOrder} from "../utils";

export function handleRoleAdminChanged(event: RoleAdminChanged): void {
    let ev = new RoleAdminChangedEvent(
        createEventID("RoleAdminChanged", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.logIndex = event.logIndex;
    ev.name = "RoleAdminChanged";
    ev.addresses = [];
    ev.role = event.params.role;
    ev.previousAdminRole = event.params.previousAdminRole;
    ev.newAdminRole = event.params.newAdminRole;
    ev.save();
}

export function handleRoleGranted(event: RoleGranted): void {
    let ev = new RoleGrantedEvent(createEventID("RoleGranted", event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.name = "RoleGranted";
    ev.addresses = [];
    ev.role = event.params.role;
    ev.account = event.params.account;
    ev.sender = event.params.sender;
    ev.save();
}
export function handleRoleRevoked(event: RoleRevoked): void {
    let ev = new RoleRevokedEvent(createEventID("RoleRevoked", event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.name = "RoleRevoked";
    ev.addresses = [];
    ev.role = event.params.role;
    ev.account = event.params.account;
    ev.sender = event.params.sender;
    ev.save();
}
