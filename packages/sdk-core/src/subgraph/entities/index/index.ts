import {
    Address,
    BigNumber,
    EntityBase,
    EntityFilterBase,
    SubgraphGetQuery,
    SubgraphId,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../../queryV2";
import {
    Index_Filter,
    Index_OrderBy,
    InputMaybe,
    Scalars,
} from "../../schema.generated";

import {
    IndexesDocument,
    IndexesQuery,
    IndexesQueryVariables,
} from "./indexes.generated";

export interface Index extends EntityBase {
    indexId: string;
    indexValue: BigNumber;
    totalAmountDistributedUntilUpdatedAt: BigNumber;
    totalSubscriptionsWithUnits: number;
    totalUnits: BigNumber;
    totalUnitsApproved: BigNumber;
    totalUnitsPending: BigNumber;
    indexCreatedEvent: SubgraphId;
    publisher: Address;
    token: Address;
}

export type IndexGetQuery = SubgraphGetQuery<Index>;

export type IndexListQuery = SubgraphListQuery<
    Index,
    IndexListQueryFilter,
    Index_OrderBy
>;

export interface IndexListQueryFilter extends EntityFilterBase {
    indexCreatedEvent?: InputMaybe<Address>;
    indexCreatedEvent_in?: InputMaybe<Array<Address>>;
    indexCreatedEvent_not_in?: InputMaybe<Array<Address>>;
    indexId?: InputMaybe<Scalars["BigInt"]>;
    indexId_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    indexId_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    indexValue?: InputMaybe<Scalars["BigInt"]>;
    indexValue_gt?: InputMaybe<Scalars["BigInt"]>;
    indexValue_gte?: InputMaybe<Scalars["BigInt"]>;
    indexValue_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    indexValue_lt?: InputMaybe<Scalars["BigInt"]>;
    indexValue_lte?: InputMaybe<Scalars["BigInt"]>;
    indexValue_not?: InputMaybe<Scalars["BigInt"]>;
    indexValue_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    publisher?: InputMaybe<Scalars["String"]>;
    publisher_in?: InputMaybe<Array<Scalars["String"]>>;
    publisher_not_in?: InputMaybe<Array<Scalars["String"]>>;
    token?: InputMaybe<Scalars["String"]>;
    token_in?: InputMaybe<Array<Scalars["String"]>>;
    token_not_in?: InputMaybe<Array<Scalars["String"]>>;
    totalAmountDistributedUntilUpdatedAt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_gt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_gte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalAmountDistributedUntilUpdatedAt_lt?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_lte?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_not?: InputMaybe<Scalars["BigInt"]>;
    totalAmountDistributedUntilUpdatedAt_not_in?: InputMaybe<
        Array<Scalars["BigInt"]>
    >;
    totalSubscriptionsWithUnits?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_gt?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_gte?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalSubscriptionsWithUnits_lt?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_lte?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_not?: InputMaybe<Scalars["Int"]>;
    totalSubscriptionsWithUnits_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    totalUnits?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_gt?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_gte?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalUnitsApproved_lt?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_lte?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_not?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsApproved_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalUnitsPending?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_gt?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_gte?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalUnitsPending_lt?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_lte?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_not?: InputMaybe<Scalars["BigInt"]>;
    totalUnitsPending_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalUnits_gt?: InputMaybe<Scalars["BigInt"]>;
    totalUnits_gte?: InputMaybe<Scalars["BigInt"]>;
    totalUnits_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    totalUnits_lt?: InputMaybe<Scalars["BigInt"]>;
    totalUnits_lte?: InputMaybe<Scalars["BigInt"]>;
    totalUnits_not?: InputMaybe<Scalars["BigInt"]>;
    totalUnits_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
}

export class IndexQueryHandler extends SubgraphQueryHandler<
    Index,
    IndexListQueryFilter,
    Index_OrderBy,
    IndexesQuery,
    Index_Filter,
    IndexesQueryVariables
> {
    // validateFilter(filter: IndexSubscriptionListQueryFilter) {}

    convertToSubgraphFilter(filter: IndexListQueryFilter): Index_Filter {
        return filter;
    }

    mapFromSubgraphResponse(response: IndexesQuery): Index[] {
        return response.indexes.map((x) => ({
            ...x,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            indexCreatedEvent: x.indexCreatedEvent.id,
            publisher: x.publisher.id,
            token: x.token.id,
        }));
    }

    requestDocument = IndexesDocument;
}
