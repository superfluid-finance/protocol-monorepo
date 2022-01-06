import {
    Address,
    BigNumber,
    EntityBase,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import {
    IndexSubscription_Filter,
    IndexSubscription_OrderBy
} from "../../schema.generated";

import {
    IndexSubscriptionsDocument,
    IndexSubscriptionsQuery,
    IndexSubscriptionsQueryVariables,
} from "./indexSubscriptions.generated";

export interface IndexSubscription extends EntityBase {
    approved: boolean;
    id: SubgraphId;
    indexValueUntilUpdatedAt: BigNumber;
    totalAmountReceivedUntilUpdatedAt: BigNumber;
    units: BigNumber;
    index: SubgraphId;
    token: Address;
    subscriber: Address;
    publisher: Address;
}

export type IndexSubscriptionOrderBy = IndexSubscription_OrderBy;

export type IndexSubscriptionsListQuery = SubgraphListQuery<
    IndexSubscriptionListQueryFilter,
    IndexSubscriptionOrderBy
>;

export type IndexSubscriptionListQueryFilter = Omit<
    IndexSubscription_Filter,
    SubgraphFilterOmitFieldList
>;

export class IndexSubscriptionQueryHandler extends SubgraphQueryHandler<
    IndexSubscription,
    IndexSubscriptionsListQuery,
    IndexSubscriptionsQuery,
    IndexSubscription_Filter,
    IndexSubscriptionsQueryVariables
> {
    // validateFilter(filter: IndexSubscriptionListQueryFilter) {
    //     validateIndexSubscriptionRequest(filter);
    // }

    convertToSubgraphFilter = (
        filter: IndexSubscriptionListQueryFilter
    ): IndexSubscription_Filter => filter;

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
        filter: IndexSubscriptionListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [],
        accounts: [
            filter.subscriber,
            filter.subscriber_in,
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
