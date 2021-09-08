import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { Account, Stream, Subscriber, Index } from "../generated/schema";

export function createEventID(event: ethereum.Event): string {
    return event.transaction.hash
        .toHex()
        .concat("-")
        .concat(event.logIndex.toString());
}

/**
 * Creates an Account entity if non exists or updates an existing one.
 * this should technically only be updated once as no property on account
 * changes currently.
 * @param id
 * @param lastModified
 * @returns created or modified account
 */
export function createOrUpdateAccount(
    id: string,
    lastModified: BigInt
): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
        account.createdAt = lastModified;
        account.updatedAt = lastModified;
    }
    account.save();
    return account;
}

function getStreamID(owner: string, recipient: string, token: string): string {
    return owner.concat("-").concat(recipient).concat("-").concat(token);
}

export function getStream(
    senderAddress: string,
    receiverAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Stream {
    let id = getStreamID(senderAddress, receiverAddress, tokenAddress);
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAt = timestamp;
        stream.token = tokenAddress;
        stream.sender = senderAddress;
        stream.receiver = receiverAddress;
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilLastUpdate = BigInt.fromI32(0);

        // Create accounts if they do not exist
        createOrUpdateAccount(senderAddress, timestamp);
        createOrUpdateAccount(receiverAddress, timestamp);
    }
    return stream;
}

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

export function getSubscriberID(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): string {
    return (
        subscriberAddress.toHexString() +
        "-" +
        publisherAddress.toHexString() +
        "-" +
        tokenAddress.toHexString() +
        "-" +
        indexId.toString()
    );
}

export function getIndexID(
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): string {
    return (
        publisherAddress.toHex() +
        "-" +
        tokenAddress.toHex() +
        "-" +
        indexId.toString()
    );
}

export function getOrInitializeIndex(
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt,
    lastModified: BigInt
): Index {
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let index = Index.load(indexEntityId);
    if (index == null) {
        let publisherId = publisherAddress.toHex();
        index = new Index(indexEntityId);
        index.createdAt = lastModified;
        index.token = tokenAddress.toHex();
        index.publisher = publisherId;
        index.indexId = indexId;
        index.oldIndexValue = BigInt.fromI32(0);
        index.newIndexValue = BigInt.fromI32(0);
        index.totalUnitsPending = BigInt.fromI32(0);
        index.totalUnitsApproved = BigInt.fromI32(0);
        index.totalUnits = BigInt.fromI32(0);
        index.totalUnitsDistributed = BigInt.fromI32(0);

        createOrUpdateAccount(publisherId, lastModified);
    }
    index.updatedAt = lastModified;
    return index;
}

export function getSubscriber(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt,
    lastModified: BigInt
): [Subscriber, boolean] {
    let subscriberEntityId = getSubscriberID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscriber = Subscriber.load(subscriberEntityId);
    if (subscriber == null) {
        let subscriberId = subscriberAddress.toHex();
        subscriber = new Subscriber(subscriberEntityId);
        subscriber.createdAt = lastModified;
        subscriber.updatedAt = lastModified;
        subscriber.token = tokenAddress.toHex();
        subscriber.subscriber = subscriberId;
        subscriber.publisher = publisherAddress.toHex();
        subscriber.indexId = indexId;
        subscriber.approved = false;
        subscriber.units = BigInt.fromI32(0);
        subscriber.totalReceivedUntilLastUpdate = BigInt.fromI32(0);
        subscriber.totalPendingApproval = BigInt.fromI32(0);
        subscriber.index = getIndexID(publisherAddress, tokenAddress, indexId);

        createOrUpdateAccount(subscriberId, lastModified);
        return [subscriber, false];
    }
    subscriber.updatedAt = lastModified;
    return [subscriber, true];
}
