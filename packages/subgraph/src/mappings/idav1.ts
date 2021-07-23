import { BigInt, Bytes, log } from "@graphprotocol/graph-ts"
import {
    IndexCreated,
    IndexUpdated,
    IndexSubscribed,
    SubscriptionApproved,
    IndexUnsubscribed,
    SubscriptionRevoked,
    IndexUnitsUpdated,
    SubscriptionUnitsUpdated } from "../../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1"
import { Subscriber, indexUpdate,indexUnitUpdate,indexUnsubscribed,subcriptionApproved,subscriptionRevoked,subscriptionUnitsUpdated, indexSubscribed } from "../../generated/schema"
import {createEventID, logTransaction,removeSubscription,fetchIndex} from '../utils'

export function handleIndexCreated(event:IndexCreated): void{
    
    let entity = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString());
    entity.totalDistribution = new BigInt(0);
    entity.totalUnits = new BigInt(0);
    entity.totalUnitsApproved = new BigInt(0);
    entity.totalUnitsPending = new BigInt(0);
    
    entity.token = event.params.token;
    entity.userData = event.params.userData;
    entity.indexId = event.params.indexId;
    entity.publisher = event.params.publisher;
    entity.save();
}

export function handleIndexSubscribed(event:IndexSubscribed): void{
    let ind = new indexSubscribed(createEventID(event));
    ind.index = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString()).id;
    ind.subscriber= event.params.subscriber;
    ind.userData=event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save()

    log.log(log.Level.INFO,"Inside IndexSubscribed");
    let entity = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString());
    // Adding the active subscriber to the index
    if(!entity.activeSubscribers.includes(event.params.subscriber as Bytes))
    {
        let l = entity.activeSubscribers.length;
        log.log(log.Level.INFO,"SubSizeIS1 "+l.toString());
        log.log(log.Level.INFO,"Adding an active subscriber. IndexSubed "+l.toString());
        log.log(log.Level.INFO,"Adding index "+entity.id);
        let activeSubscribers = entity.activeSubscribers;
        activeSubscribers.push(event.params.subscriber as Bytes);
        entity.activeSubscribers = activeSubscribers;
        
        l = entity.activeSubscribers.length;
        log.log(log.Level.INFO,"SubSizeIS2 "+l.toString());
    }
    entity.save();
}

export function handleIndexUnitsUpdated(event:IndexUnitsUpdated): void{
    let ind = new indexUnitUpdate(createEventID(event));
    ind.index = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString()).id;
    ind.units = event.params.units;
    ind.subscriber= event.params.subscriber;
    ind.units=event.params.units;
    ind.userData=event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save()

    log.log(log.Level.INFO,"Inside IndexUnitsUpdated");
    let entity = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString());
    // Adding the active subscriber to the index
    if(!entity.activeSubscribers.includes(event.params.subscriber as Bytes)&&event.params.units>new BigInt(0))
    {
        let l = entity.activeSubscribers.length;
        log.log(log.Level.INFO,"SubSize "+l.toString());
        log.log(log.Level.INFO,"Adding an active subscriber "+event.params.subscriber.toHexString());
        log.log(log.Level.INFO,"Adding index "+entity.id);
        let activeSubscribers = entity.activeSubscribers;
        activeSubscribers.push(event.params.subscriber as Bytes);
        entity.activeSubscribers = activeSubscribers;
        
        l = entity.activeSubscribers.length;
        log.log(log.Level.INFO,"SubSize2 "+l.toString());
    }
    entity.save();
}

export function handleIndexUnsubscribed(event:IndexUnsubscribed): void{
    let ind = new indexUnsubscribed(createEventID(event));
    ind.transaction = logTransaction(event).id;
    ind.index = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString()).id;
    ind.subscriber = event.params.subscriber;
    ind.userData = event.params.userData;
    ind.save();

    log.log(log.Level.INFO,"Removing an active subscriber "+event.params.subscriber.toHexString());
    let entity = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString());
    entity.activeSubscribers = removeSubscription(entity.activeSubscribers as Bytes[],event.params.subscriber);
    entity.save();
}

export function handleIndexUpdated(event:IndexUpdated): void{
    let thisDistribution = event.params.newIndexValue.minus(event.params.oldIndexValue).times(event.params.totalUnitsPending.plus(event.params.totalUnitsApproved));
    let ind = new indexUpdate(createEventID(event));
    ind.index = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString()).id;
    ind.newIndexValue = event.params.newIndexValue;
    ind.oldIndexValue = event.params.oldIndexValue;
    ind.totalUnitsApproved = event.params.totalUnitsApproved;
    ind.totalUnitsPending = event.params.totalUnitsPending;
    ind.userData = event.params.userData;
    ind.distribution = thisDistribution;
    ind.transaction = logTransaction(event).id;
    ind.save();

    let entity = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString());
    entity.newIndexValue = event.params.newIndexValue
    entity.oldIndexValue = event.params.oldIndexValue
    entity.totalUnitsApproved = event.params.totalUnitsApproved
    entity.totalUnitsPending = event.params.totalUnitsPending
    entity.totalUnits = event.params.totalUnitsApproved.plus(event.params.totalUnitsPending);
    entity.userData = event.params.userData
    entity.totalDistribution = entity.totalDistribution.plus(thisDistribution);
    entity.save()

    // Code for calculating distribution for each subscriber.
    if(!entity.totalUnits.equals(new BigInt(0))&&!entity.totalUnits.equals(null))
    {
        let l = entity.activeSubscribers.length;
        log.log(log.Level.INFO,"Inside per calculation "+l.toString())

        var perUnit = thisDistribution.div(entity.totalUnits as BigInt) as BigInt;//Divide by zero handling

        if(entity.activeSubscribers.length>0){
            log.log(log.Level.INFO,"Calculating distribution for subscribers "+l.toString());
            entity.activeSubscribers.forEach(element => {
                log.log(log.Level.DEBUG,"DEBUG"+element.toString())
                let element2 = Subscriber.load(element.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
                if(element2!=null){
                    element2.totalReceived = element2.totalReceived.plus(perUnit.times(element2.units as BigInt));
                    element2.save();
                } 
            });
        }
    }
}

export function handleSubscriptionApproved(event:SubscriptionApproved): void{
    let entity = Subscriber.load(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
    if(entity==null){
        entity =new Subscriber(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
        entity.revoked = false;
        entity.totalReceived = new BigInt(0);
    }
    entity.approved = true;
    entity.publisher = event.params.publisher;
    entity.indexId = event.params.indexId;
    entity.subscriber = event.params.subscriber;
    entity.token = event.params.token;
    entity.userData =event.params.userData;
    entity.index = fetchIndex(event.params.publisher.toHexString()+"-"+event.params.token.toHexString()+"-"+event.params.indexId.toHexString()).id;
    entity.save()

    let ind = new subcriptionApproved(createEventID(event));
    ind.subscription = Subscriber.load(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString()).id;
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save();
}

export function handleSubscriptionRevoked(event:SubscriptionRevoked): void{
    let ind = new subscriptionRevoked(createEventID(event));
    ind.subscription = Subscriber.load(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString()).id;
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save();

    let entity = Subscriber.load(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
    entity.userData =event.params.userData
    entity.revoked = true
    entity.save()
}

export function handleSubscriptionUnitsUpdated(event:SubscriptionUnitsUpdated): void{

    let entity = Subscriber.load(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
    if(entity==null){
        entity =new Subscriber(event.params.subscriber.toHexString()+event.params.indexId.toHexString()+event.params.publisher.toHexString()+event.params.token.toHexString());
        entity.subscriber = event.params.subscriber;
        entity.publisher=  event.params.publisher;
        entity.token = event.params.token;
        entity.indexId = event.params.indexId;
        entity.approved = false;
        entity.revoked = false;
        entity.totalReceived = new BigInt(0);
    }
    entity.units = event.params.units;
    entity.save();
    
    let ind = new subscriptionUnitsUpdated(createEventID(event));
    ind.subscription = entity.id;
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.units = event.params.units;
    ind.save();
}
