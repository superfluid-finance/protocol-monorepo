import {
    BigInt,
    BigDecimal,
    EthereumEvent,
    Address,
    log,
} from "@graphprotocol/graph-ts";

import { Account, Flow, Token, Transaction } from "../generated/schema";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";

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
    return owner
        .concat("-")
        .concat(recipient)
        .concat("-")
        .concat(token);
}

export function fetchToken(address: string): Token {
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
    }
    return token as Token;
}

export function fetchFlow(
    owner: string,
    recipient: string,
    token: string,
    timestamp: BigInt
): Flow {
    let id = createFlowID(owner, recipient, token);
    let flow = Flow.load(id);
    if (flow == null) {
        flow = new Flow(id);
        flow.sum = BigDecimal.fromString("0");
        flow.flowRate = BigInt.fromI32(0);
        flow.token = token;
        flow.owner = owner;
        flow.lastUpdate = timestamp;
        flow.recipient = recipient;

        // Create accounts if they do not exist
        let ownerAccount = fetchAccount(owner);
        let recipientAccount = fetchAccount(recipient);
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
