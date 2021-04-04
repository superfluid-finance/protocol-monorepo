import {
    BigInt,
    BigDecimal,
    EthereumEvent,
    Address,
    log,
} from "@graphprotocol/graph-ts";

import { SuperToken as SuperTokenTemplate } from "../generated/templates";
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
        // Create a dynamic data source instance
        // https://thegraph.com/docs/define-a-subgraph#instantiating-a-data-source-template
        SuperTokenTemplate.create(Address.fromString(address));
    }
    return token as Token;
}

export function fetchFlow(
    owner: string,
    recipient: string,
    tokenAddress: string,
    timestamp: BigInt
): Flow {
    let id = createFlowID(owner, recipient, tokenAddress);
    let flow = Flow.load(id);
    if (flow == null) {
        flow = new Flow(id);
        flow.sum = BigDecimal.fromString("0");
        flow.flowRate = BigInt.fromI32(0);
        flow.token = tokenAddress;
        flow.owner = owner;
        flow.lastUpdate = timestamp;
        flow.recipient = recipient;

        // Create accounts/tokens if they do not exist
        let token = fetchToken(tokenAddress);
        token.save();
        let ownerAccount = fetchAccount(owner);
        ownerAccount.save();
        let recipientAccount = fetchAccount(recipient);
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
