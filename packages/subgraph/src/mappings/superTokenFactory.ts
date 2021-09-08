import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import { ISuperToken as SuperToken } from "../../generated/templates/SuperToken/ISuperToken";
import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
    Token,
} from "../../generated/schema";
import { createEventID } from "../utils";

/**
 * Creates a HOL Token (SuperToken) entity if non exists, this function should
 * never be called more than once for the Token entity (you only create a
 * SuperToken once).
 * @param address
 * @param lastModified
 * @returns created token
 */
function createOrUpdateToken(address: string, lastModified: BigInt): Token {
    let token = Token.load(address);
    if (token == null) {
        let tokenContract = SuperToken.bind(Address.fromString(address));
        let underlyingAddress = tokenContract.getUnderlyingToken();
        let name = tokenContract.name();
        let symbol = tokenContract.symbol();
        token = new Token(address);
        token.underlyingAddress = underlyingAddress;
        token.name = name;
        token.symbol = symbol;
        token.createdAt = lastModified;
        // NOTE: deleted template code, not sure what this was doing
    }
    token.updatedAt = lastModified;
    token.save();
    return token;
}

export function handleSuperTokenCreated(event: SuperTokenCreatedEvent): void {
    let tokenAddress = event.params.token.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new SuperTokenCreated(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = tokenAddress;
    ev.save();
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    let tokenAddress = event.params.token.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new CustomSuperTokenCreated(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = tokenAddress;
    ev.save();
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    let tokenAddress = event.params.tokenLogic.toHex();
    createOrUpdateToken(tokenAddress, event.block.timestamp);

    let ev = new SuperTokenLogicCreated(createEventID(event));
    ev.blockNumber = event.block.number;
    ev.timestamp = event.block.timestamp;
    ev.transactionHash = event.transaction.hash;
    ev.token = tokenAddress;
    ev.save();
}
