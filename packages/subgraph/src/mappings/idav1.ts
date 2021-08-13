import { BigInt, Bytes, log } from "@graphprotocol/graph-ts"

import {
    
    IndexCreated,
    IndexUpdated,
    IndexSubscribed,
    SubscriptionApproved,
    IndexUnsubscribed,
    SubscriptionRevoked,
    IndexUnitsUpdated,
    SubscriptionUnitsUpdated, } from "../../generated/IInstantDistributionAgreementV1/IInstantDistributionAgreementV1"
import { indexUpdate,indexUnitUpdate,indexUnsubscribed,subscriptionApproved,subscriptionRevoked,subscriptionUnitsUpdated, indexSubscribed, Subscriber } from "../../generated/schema"
import {createEventID, logTransaction,removeSubscription,fetchIndex,fetchSubscriber} from '../utils'


export function handleIndexCreated(event:IndexCreated): void{
    
    let entity = fetchIndex(event.params.publisher,event.params.token,event.params.indexId);
    entity.token = event.params.token;
    entity.userData = event.params.userData;
    entity.indexId = event.params.indexId;
    entity.publisher = event.params.publisher;
    entity.save();
}

export function handleIndexSubscribed(event:IndexSubscribed): void{
    let ind = new indexSubscribed(createEventID(event));
    ind.index = fetchIndex(event.params.publisher,event.params.token,event.params.indexId).id;
    ind.subscriber= event.params.subscriber;
    ind.userData=event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save()

    // let entity = fetchIndex(event.params.publisher,event.params.token,event.params.indexId);
    // Adding the active subscriber to the index
    // if(!entity.activeSubscribers.includes(event.params.subscriber as Bytes))
    // {
    //     var activeSubscribers = entity.activeSubscribers;
    //     var newSubscriber = event.params.subscriber as Bytes
    //     activeSubscribers.push(newSubscriber);
    //     entity.activeSubscribers = activeSubscribers;
    // }
    // entity.save();
}

export function handleIndexUnitsUpdated(event:IndexUnitsUpdated): void{
    let ind = new indexUnitUpdate(createEventID(event));
    ind.index = fetchIndex(event.params.publisher,event.params.token,event.params.indexId).id;
    ind.units = event.params.units;
    ind.subscriber= event.params.subscriber;
    ind.units=event.params.units;
    ind.userData=event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save()

    // let entity = fetchIndex(event.params.publisher,event.params.token,event.params.indexId);
    // Adding the active subscriber to the index
    // if(!entity.activeSubscribers.includes(event.params.subscriber as Bytes)&&event.params.units>new BigInt(0))//We are also comparing to greater than zero as this function is called when revoke happens
    // {
    //     var activeSubscribers = entity.activeSubscribers;
    //     var newSubscriber = event.params.subscriber as Bytes
    //     activeSubscribers.push(newSubscriber);
    //     entity.activeSubscribers = activeSubscribers;
    // }
    // entity.save();
}

export function handleIndexUnsubscribed(event:IndexUnsubscribed): void{
    let ind = new indexUnsubscribed(createEventID(event));
    ind.transaction = logTransaction(event).id;
    ind.index = fetchIndex(event.params.publisher,event.params.token,event.params.indexId).id;
    ind.subscriber = event.params.subscriber;
    ind.userData = event.params.userData;
    ind.save();
    
    // let entity = fetchIndex(event.params.publisher,event.params.token,event.params.indexId);
    // entity.activeSubscribers = removeSubscription(entity.activeSubscribers as Bytes[],event.params.subscriber);
    // entity.save();
}

export function handleIndexUpdated(event:IndexUpdated): void{
    let thisDistribution = event.params.newIndexValue.minus(event.params.oldIndexValue).times(event.params.totalUnitsPending.plus(event.params.totalUnitsApproved));
    let ind = new indexUpdate(createEventID(event));
    ind.index = fetchIndex(event.params.publisher,event.params.token,event.params.indexId).id;
    ind.newIndexValue = event.params.newIndexValue;
    ind.oldIndexValue = event.params.oldIndexValue;
    ind.totalUnitsApproved = event.params.totalUnitsApproved;
    ind.totalUnitsPending = event.params.totalUnitsPending;
    ind.userData = event.params.userData;
    ind.distribution = thisDistribution;
    ind.transaction = logTransaction(event).id;
    ind.save();

    let entity = fetchIndex(event.params.publisher,event.params.token,event.params.indexId);
    entity.newIndexValue = event.params.newIndexValue
    entity.oldIndexValue = event.params.oldIndexValue
    entity.totalUnitsApproved = event.params.totalUnitsApproved
    entity.totalUnitsPending = event.params.totalUnitsPending
    entity.totalUnits = event.params.totalUnitsApproved.plus(event.params.totalUnitsPending);
    entity.userData = event.params.userData
    entity.totalDistribution = entity.totalDistribution.plus(thisDistribution);
    entity.save()

    // Code for calculating distribution for each subscriber.
    // if(!entity.totalUnits.equals(new BigInt(0))&&!entity.totalUnits.equals(null))
    // {
    //     let l = entity.activeSubscribers.length;
    //     var perUnit = thisDistribution.div(entity.totalUnits as BigInt) as BigInt;//Divide by zero handling
    //     if(entity.activeSubscribers.length>0){
    //         for (let index = 0; index < entity.activeSubscribers.length; index++) {
    //             let elements = entity.activeSubscribers as Bytes[];
    //             let element = elements[index] as Bytes;
    //             let element2 = fetchSubscriber(element,event.params.publisher,event.params.token,event.params.indexId) as Subscriber;
    //             if (element2!=null)
    //             {
    //                 if(element2.approved){
    //                     let totalReceived = element2.totalReceived;
    //                     let tots = perUnit.times(element2.units as BigInt)
    //                     totalReceived = totalReceived.plus(tots)
    //                     element2.totalReceived = totalReceived;
    //                 }else{
    //                     let totalPendingApproval = element2.totalPendingApproval;
    //                     let tots = perUnit.times(element2.units as BigInt)
    //                     totalPendingApproval = totalPendingApproval.plus(tots)
    //                     element2.totalPendingApproval = totalPendingApproval;
    //                 }
    //                 element2.save()
    //             }
    //         }
    //     }
    // }
}

export function handleSubscriptionApproved(event:SubscriptionApproved): void{
    let entity = fetchSubscriber(event.params.subscriber,event.params.publisher,event.params.token,event.params.indexId);
    entity.approved = true;
    entity.userData =event.params.userData;
    let totalPendingApproval = entity.totalPendingApproval;
    entity.totalReceived = entity.totalReceived.plus(totalPendingApproval as BigInt)
    entity.totalPendingApproval = new BigInt(0);
    entity.save()

    let ind = new subscriptionApproved(createEventID(event));
    ind.subscriber = entity.id
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save();
}

export function handleSubscriptionRevoked(event:SubscriptionRevoked): void{
    let entity = fetchSubscriber(event.params.subscriber,event.params.publisher,event.params.token,event.params.indexId);
    entity.userData =event.params.userData;
    entity.approved=false;
    entity.save()

    let ind = new subscriptionRevoked(createEventID(event));
    ind.subscriber = entity.id
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.save();

}

export function handleSubscriptionUnitsUpdated(event:SubscriptionUnitsUpdated): void{
    let entity = fetchSubscriber(event.params.subscriber,event.params.publisher,event.params.token,event.params.indexId);
    entity.units = event.params.units;
    entity.save();
    
    let ind = new subscriptionUnitsUpdated(createEventID(event));
    ind.subscriber = entity.id;
    ind.userData = event.params.userData;
    ind.transaction = logTransaction(event).id;
    ind.units = event.params.units;
    ind.save();
}
