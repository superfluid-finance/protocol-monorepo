import { Address, Bytes, ethereum } from "@graphprotocol/graph-ts";
import {
    NameSet,
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
import { createEventID, getOrder, ZERO_ADDRESS } from "../utils";

export function handleRoleAdminChanged(event: RoleAdminChanged): void {
    let ev = new RoleAdminChangedEvent(
        createEventID("RoleAdminChanged", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
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
    ev.gasPrice = event.transaction.gasPrice;
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
    ev.gasPrice = event.transaction.gasPrice;
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

export function handleSet(event: Set): void {
    _handleSetEventHelper(event, event.params.target, event.params.name);
}

export function handleNameSet(event: NameSet): void {
    _handleSetEventHelper(event, event.params.target, event.params.name);
}

/**
 * Creates Set Event entity AND Resolver Entry
 * Also lists/unlists SuperTokens.
 * This is a shared function used by handleSet/handleNameSet because Set used to be NameSet.
 */
function _handleSetEventHelper(
    event: ethereum.Event,
    target: Address,
    name: Bytes
): void {
    _createSetEvent(event, target, name);

    const resolverEntry = getOrInitResolverEntry(
        name.toHex(),
        target,
        event.block
    );

    if (resolverEntry.isToken) {
        const token = Token.load(resolverEntry.targetAddress.toHex());
        if (token) {
            if (target.equals(ZERO_ADDRESS)) {
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
    const ev = new SetEvent(createEventID("Set", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.name = "Set";
    ev.addresses = [target];

    ev.hashedName = name;
    ev.target = target;
    ev.resolverEntry = name.toHex();
    ev.save();
}
