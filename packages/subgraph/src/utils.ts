import {
    BigInt,
    BigDecimal,
    EthereumEvent,
    log,
} from "@graphprotocol/graph-ts";

import { Account, Flow, Token, Transaction } from "../generated/schema";

export function createEventID(event: EthereumEvent): string {
    return event.block.number
        .toString()
        .concat("-")
        .concat(event.logIndex.toString());
}

export function fetchAccount(id: string): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
    }
    return account as Account;
}

function createFlowID(owner: string, recipient: string, token: string): string {
    return from
        .concat("-")
        .concat(to)
        .concat("-")
        .concat(token);
}

export function fetchToken(address: string): Token {
    let token = Token.load(address);
    if (token == null) {
        token = new Token(address);
        // TODO: fetch underlyingAddress
        // token.underlyingAddress =
    }
    return token as Token;
}

export function fetchFlow(
    owner: string,
    recipient: string,
    token: string
): Flow {
    let id = createFlowID(owner, recipient, token);
    let flow = Flow.load(id);
    if (flow == null) {
        flow = new Flow(id);
        flow.sum = BigDecimal.fromString("0");
        flow.flowRate = BigDecimal.fromString("0");
        flow.token = token;
        flow.owner = owner;
        flow.recipient = recipient;

        // Update accounts
        let ownerAccount = fetchAccount(owner);
        let recipientAccount = fetchAccount(recipient);
        let flowsOwned = onwerAccount.flowsOwned;
        let flowsReceived = recipientAccount.flowsReceived;
        flowsOwned.push(id);
        flowsReceived.push(id);
        ownerAccount.flowsOwner = flowsOwned;
        recipientAccount.flowsReceived = flowsReceived;
        ownerAccount.save();
        recipientAccount.save();
    }
    return flow as Flow;
}

export function logTransaction(event: EthereumEvent): Transaction {
    let tx = new Transaction(event.transaction.hash.toHex());
    tx.timestamp = event.block.timestamp;
    tx.blockNumber = event.block.number;
    tx.save();
    return tx as Transaction;
}

export function toDai(value: BigInt): BigDecimal {
    return value.divDecimal(BigDecimal.fromString("1000000000000000000")); // 18 decimal
}
