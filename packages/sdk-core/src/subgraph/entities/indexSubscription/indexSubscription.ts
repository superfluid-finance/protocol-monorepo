import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import {
    IndexSubscription_Filter,
    IndexSubscription_OrderBy,
} from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    IndexSubscriptionsDocument,
    IndexSubscriptionsQuery,
    IndexSubscriptionsQueryVariables,
} from "./indexSubscriptions.generated";

export interface IndexSubscription {
    id: SubgraphId;
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    updatedAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
    approved: boolean;
    indexValueUntilUpdatedAt: BigNumber;
    totalAmountReceivedUntilUpdatedAt: BigNumber;
    units: BigNumber;
    index: SubgraphId;
    token: Address;
    subscriber: Address;
    publisher: Address;
}

export type IndexSubscriptionsListQuery = SubgraphListQuery<
    IndexSubscription_Filter,
    IndexSubscription_OrderBy
>;

export class IndexSubscriptionQueryHandler extends SubgraphQueryHandler<
    IndexSubscription,
    IndexSubscriptionsListQuery,
    IndexSubscriptionsQuery,
    IndexSubscriptionsQueryVariables
> {
    mapFromSubgraphResponse = (
        response: IndexSubscriptionsQuery
    ): IndexSubscription[] =>
        response.indexSubscriptions.map((x) => ({
            ...x,
            subscriber: x.subscriber.id,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            index: x.index.id,
            token: x.index.token.id,
            publisher: x.index.publisher.id,
        }));

    requestDocument = IndexSubscriptionsDocument;

    protected getRelevantAddressesFromFilterCore = (
        filter: IndexSubscription_Filter
    ): RelevantAddressesIntermediate => ({
        tokens: [],
        accounts: [
            filter.subscriber,
            filter.subscriber_in,
            filter.subscriber_not,
            filter.subscriber_not_in,
        ],
    });

    protected getRelevantAddressesFromResultCore = (
        result: IndexSubscription
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.subscriber, result.publisher],
    });
}
