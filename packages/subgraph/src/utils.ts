import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";

import { SuperToken as SuperTokenTemplate } from "../generated/templates";
import {
    Account,
    Stream,
    Token,
    Transaction,
    AccountInteractedToken,
    Subscriber,
    Index,
} from "../generated/schema";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { SubscriptionApproved } from "../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1";

export function createEventID(event: ethereum.Event): string {
    return event.transaction.hash
        .toHex()
        .concat("-")
        .concat(event.logIndex.toString());
}

export function createAndReturnTxn(event: ethereum.Event): Transaction {
    let tx = new Transaction(event.transaction.hash.toHex());
    tx.timestamp = event.block.timestamp;
    tx.blockNumber = event.block.number;
    tx.save();
    return tx as Transaction;
}

export function fetchAccount(id: string): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
    }
    return account as Account;
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
        SuperTokenTemplate.create(Address.fromString(address));
    }
    return token as Token;
}

export function fetchAccountInteractedToken(
    accountId: string,
    tokenId: string
): AccountInteractedToken {
    let id = accountId.concat("-").concat(tokenId);
    let accountWithToken = AccountInteractedToken.load(id);
    if (accountWithToken == null) {
        // NOTE: removed fetchAccount + save as we do this prior to calling this function
        // everywhere in the code.
        accountWithToken = new AccountInteractedToken(id);
        accountWithToken.account = accountId;
        accountWithToken.token = tokenId;
    }
    return accountWithToken as AccountInteractedToken;
}

function createStreamID(
    owner: string,
    recipient: string,
    token: string
): string {
    return owner.concat("-").concat(recipient).concat("-").concat(token);
}

export function fetchStream(
    senderAddress: string,
    receiverAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Stream {
    let id = createStreamID(senderAddress, receiverAddress, tokenAddress);
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.token = tokenAddress;
        stream.sender = senderAddress;
        stream.receiver = receiverAddress;
        stream.lastUpdate = timestamp;
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilLastUpdate = BigInt.fromI32(0);

        // Create accounts and tokens if they do not exist
        // NOTE: Not for tokens, it is impossible to start a stream if the specified token doesn't exist.
        let ownerAccount = fetchAccount(senderAddress);
        let receiverAccount = fetchAccount(receiverAddress);
        ownerAccount.save();
        receiverAccount.save();
    }
    return stream as Stream;
}

// is create account necessary? look at all the places calling updateBalance
export function updateBalance(accountId: string, tokenId: string): void {
    let accountInteractedToken = fetchAccountInteractedToken(
        accountId,
        tokenId
    );
    log.info("Token updateBalance: {}", [tokenId]);
    let superTokenContract = SuperToken.bind(Address.fromString(tokenId));
    let newBalance = superTokenContract.balanceOf(
        Address.fromString(accountId)
    );
    accountInteractedToken.balance = newBalance.toBigDecimal();
    accountInteractedToken.save();
    return;
}

export function createSubscriptionID(event: SubscriptionApproved): string {
    return (
        event.params.subscriber.toHexString() +
        event.params.publisher.toHexString() +
        event.params.indexId.toHexString() +
        event.params.token.toHexString()
    );
}

export function fetchIndex(
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): Index {
    let index = Index.load(
        publisherAddress.toHexString() +
            "-" +
            tokenAddress.toHexString() +
            "-" +
            indexId.toHexString()
    );
    if (index == null) {
        index = new Index(
            publisherAddress.toHexString() +
                "-" +
                tokenAddress.toHexString() +
                "-" +
                indexId.toHexString()
        );
        index.totalDistribution = new BigInt(0);
        index.totalUnits = new BigInt(0);
        index.totalUnitsApproved = new BigInt(0);
        index.totalUnitsPending = new BigInt(0);
    }
    return index as Index;
}

// TODO: abstract out the getSubscriberId
export function fetchSubscriber(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): Subscriber {
    let subscriber = Subscriber.load(
        subscriberAddress.toHexString() +
            "-" +
            publisherAddress.toHexString() +
            "-" +
            tokenAddress.toHexString() +
            "-" +
            indexId.toHexString()
    );
    if (subscriber == null) {
        subscriber = new Subscriber(
            subscriberAddress.toHexString() +
                "-" +
                publisherAddress.toHexString() +
                "-" +
                tokenAddress.toHexString() +
                "-" +
                indexId.toHexString()
        );
        subscriber.subscriber = subscriberAddress;
        subscriber.publisher = publisherAddress;
        subscriber.token = tokenAddress;
        subscriber.indexId = indexId;
        subscriber.approved = false;
        subscriber.totalReceived = new BigInt(0);
        subscriber.totalPendingApproval = new BigInt(0);
        subscriber.units = new BigInt(0);
        subscriber.index = fetchIndex(
            publisherAddress,
            tokenAddress,
            indexId
        ).id;
    }
    return subscriber as Subscriber;
}

export function removeSubscription(subscribers: Bytes[], sub: Bytes): Bytes[] {
    let temp: Bytes[] = [];
    var ss = sub.toHexString();
    for (let index = 0; index < subscribers.length; index++) {
        let element = subscribers[index].toHexString();
        if (ss != element) {
            temp.push(subscribers[index]);
        }
    }
    return temp;
}
