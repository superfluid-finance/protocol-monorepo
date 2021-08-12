import {
    BigInt,
    BigDecimal,
    EthereumEvent,
    Address,
    log,
    Bytes,
} from "@graphprotocol/graph-ts";

import { SuperToken as SuperTokenTemplate } from "../generated/templates";
import { Account, Flow, Token, Transaction,Index,Subscriber  } from "../generated/schema";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { SubscriptionApproved} from "../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1"

export function createEventID(event: EthereumEvent): string {
    return event.block.number
        .toString()
        .concat("-")
        .concat(event.logIndex.toString());
}

function createFlowID(owner: string, recipient: string, token: string): string {
    return owner
        .concat("-")
        .concat(recipient)
        .concat("-")
        .concat(token);
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
        token.name = "foo";
        token.symbol = "foo";
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
    ownerAddress: string,
    recipientAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Flow {
    let id = createFlowID(ownerAddress, recipientAddress, tokenAddress);
    let flow = Flow.load(id);
    if (flow == null) {
        flow = new Flow(id);
        flow.sum = BigDecimal.fromString("0");
        flow.flowRate = BigInt.fromI32(0);
        let token = fetchToken(tokenAddress);
        flow.token = token.id;
        flow.owner = ownerAddress;
        flow.lastUpdate = timestamp;
        flow.recipient = recipientAddress;

        // Create accounts and tokens if they do not exist
        let ownerAccount = fetchAccount(ownerAddress);
        let recipientAccount = fetchAccount(recipientAddress);
        ownerAccount.save();
        recipientAccount.save();
        token.save();
    }
    return flow as Flow;
}

//IDA

export function createSubscriptionID(event: SubscriptionApproved): string {
    return event.params.subscriber.toHexString()+event.params.publisher.toHexString()+event.params.indexId.toHexString()+event.params.token.toHexString()
}

export function fetchIndex(publisher:Bytes,token:Bytes,indexId:BigInt):Index{
    let entity = Index.load(publisher.toHexString()+"-"+token.toHexString()+"-"+indexId.toHexString());
    if(entity==null){
        entity = new Index(publisher.toHexString()+"-"+token.toHexString()+"-"+indexId.toHexString())
        entity.totalDistribution = new BigInt(0);
        entity.totalUnits = new BigInt(0);
        entity.totalUnitsApproved = new BigInt(0);
        entity.totalUnitsPending = new BigInt(0);
    }
    return entity as Index;
}

export function fetchSubscriber(subscriber:Bytes,publisher:Bytes,token:Bytes,indexId:BigInt):Subscriber{
    let entity = Subscriber.load(subscriber.toHexString()+"-"+publisher.toHexString()+"-"+token.toHexString()+"-"+indexId.toHexString());
    if(entity==null){
        entity = new Subscriber(subscriber.toHexString()+"-"+publisher.toHexString()+"-"+token.toHexString()+"-"+indexId.toHexString())
        entity.subscriber = subscriber;
        entity.publisher=  publisher;
        entity.token = token;
        entity.indexId = indexId;
        entity.approved = false;
        entity.totalReceived = new BigInt(0);
        entity.totalPendingApproval =new BigInt(0); 
        entity.units = new BigInt(0);
        entity.index = fetchIndex(publisher,token,indexId).id;
    }
    return entity as Subscriber;
}

export function removeSubscription(subscribers: Bytes[],sub:Bytes): Bytes[] {
    let temp:Bytes[]=[]
    var ss =sub.toHexString()
    for (let index = 0; index < subscribers.length; index++) {
        let element = subscribers[index].toHexString();
        if(ss!=element){
            temp.push(subscribers[index])
        }
    }
    return temp;
}
