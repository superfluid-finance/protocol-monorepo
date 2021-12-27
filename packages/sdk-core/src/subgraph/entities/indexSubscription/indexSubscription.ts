import {
    Address,
    AddressInput,
    BigNumber,
    BigNumberInput,
    EntityBase,
    EntityFilterBase,
    RelevantAddressesIntermediate,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    TimestampInput,
} from "../../../queryV2";
import {
    IndexSubscription_Filter,
    IndexSubscription_OrderBy,
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

export type IndexSubscriptionsListQuery = SubgraphListQuery<
    IndexSubscriptionListQueryFilter,
    IndexSubscription_OrderBy
>;

export interface IndexSubscriptionListQueryFilter extends EntityFilterBase {
    approved?: boolean;
    approved_in?: boolean[];
    approved_not?: boolean;
    approved_not_in?: boolean[];
    indexValueUntilUpdatedAt?: BigNumberInput;
    indexValueUntilUpdatedAt_gt?: BigNumberInput;
    indexValueUntilUpdatedAt_gte?: BigNumberInput;
    indexValueUntilUpdatedAt_in?: BigNumberInput[];
    indexValueUntilUpdatedAt_lt?: BigNumberInput;
    indexValueUntilUpdatedAt_lte?: BigNumberInput;
    indexValueUntilUpdatedAt_not?: BigNumberInput;
    indexValueUntilUpdatedAt_not_in?: BigNumberInput[];
    // index_contains?: string;
    // index_ends_with?: string;
    // index_gt?: InputMaybe<Scalars['String']>;
    // index_gte?: InputMaybe<Scalars['String']>;
    // index_in?: InputMaybe<Array<Scalars['String']>>;
    // index_lt?: InputMaybe<Scalars['String']>;
    // index_lte?: InputMaybe<Scalars['String']>;
    // index_not?: InputMaybe<Scalars['String']>;
    // index_not_contains?: InputMaybe<Scalars['String']>;
    // index_not_ends_with?: InputMaybe<Scalars['String']>;
    // index_not_in?: InputMaybe<Array<Scalars['String']>>;
    // index_not_starts_with?: InputMaybe<Scalars['String']>;
    // index_starts_with?: InputMaybe<Scalars['String']>;
    subscriber?: AddressInput;
    subscriber_in?: AddressInput[];
    subscriber_not_in?: AddressInput[];
    totalAmountReceivedUntilUpdatedAt?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_gt?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_gte?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_in?: TimestampInput[];
    totalAmountReceivedUntilUpdatedAt_lt?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_lte?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_not?: TimestampInput;
    totalAmountReceivedUntilUpdatedAt_not_in?: TimestampInput[];
    units?: BigNumberInput;
    units_gt?: BigNumberInput;
    units_gte?: BigNumberInput;
    units_in?: BigNumberInput[];
    units_lt?: BigNumberInput;
    units_lte?: BigNumberInput;
    units_not?: BigNumberInput;
    units_not_in?: BigNumberInput[];
}

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
        filter: IndexSubscriptionsListQuery["filter"]
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
