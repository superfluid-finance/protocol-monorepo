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
    indexValueCurrent: BigNumber;
    totalAmountReceivedUntilUpdatedAt: BigNumber;
    units: BigNumber;
    indexTotalUnits: BigNumber;
    index: SubgraphId;
    indexId: string;
    token: Address;
    tokenSymbol: string;
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
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof IndexSubscription_Filter)[];
        tokenKeys: (keyof IndexSubscription_Filter)[];
    } => ({
        accountKeys: ["subscriber"],
        tokenKeys: [],
    });

    getRelevantAddressesFromResultCore = (
        result: IndexSubscription
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.subscriber, result.publisher],
    });

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
            indexId: x.index.indexId,
            indexValueCurrent: x.index.indexValue,
            indexTotalUnits: x.index.totalUnits,
            token: x.index.token.id,
            tokenSymbol: x.index.token.symbol,
            publisher: x.index.publisher.id,
        }));

    requestDocument = IndexSubscriptionsDocument;
}
