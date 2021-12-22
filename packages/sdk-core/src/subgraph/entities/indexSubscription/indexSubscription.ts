import {
    Address,
    AddressInput,
    BigNumber,
    BigNumberInput,
    EntityBase,
    EntityFilterBase,
    SubgraphGetQuery,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
    TimestampInput,
} from "../../../queryV2";
import {
    GetIndexSubscriptionsDocument,
    GetIndexSubscriptionsQuery,
    GetIndexSubscriptionsQueryVariables,
} from "../../queries/getIndexSubscriptions.generated";
import {
    IndexSubscription_Filter,
    IndexSubscription_OrderBy,
} from "../../schema.generated";

export interface IndexSubscription extends EntityBase {
    approved: boolean;
    id: SubgraphId;
    indexValueUntilUpdatedAt: BigNumber;
    totalAmountReceivedUntilUpdatedAt: BigNumber;
    units: BigNumber;
    index: SubgraphId;
    subscriber: Address;
}

export type IndexSubscriptionGetQuery = SubgraphGetQuery<IndexSubscription>;

export type IndexSubscriptionsListQuery = SubgraphListQuery<
    IndexSubscription,
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
    IndexSubscriptionListQueryFilter,
    IndexSubscription_OrderBy,
    GetIndexSubscriptionsQuery,
    IndexSubscription_Filter,
    GetIndexSubscriptionsQueryVariables
> {
    // validateFilter(filter: IndexSubscriptionListQueryFilter) {
    //     validateIndexSubscriptionRequest(filter);
    // }

    convertToSubgraphFilter(
        filter: IndexSubscriptionListQueryFilter
    ): IndexSubscription_Filter {
        return filter;
    }

    mapFromSubgraphResponse(
        response: GetIndexSubscriptionsQuery
    ): IndexSubscription[] {
        return response.result.map((x) => ({
            ...x,
            subscriber: x.subscriber.id,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            index: x.index.id,
        }));
    }

    requestDocument = GetIndexSubscriptionsDocument;
}
