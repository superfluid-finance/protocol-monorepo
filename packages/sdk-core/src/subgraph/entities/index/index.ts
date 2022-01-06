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
import { Index_Filter, Index_OrderBy } from "../../schema.generated";

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

export type IndexOrderBy = Index_OrderBy;

export type IndexListQuery = SubgraphListQuery<
    IndexListQueryFilter,
    IndexOrderBy
>;

export type IndexListQueryFilter = Omit<
    Index_Filter,
    SubgraphFilterOmitFieldList
>;

export class IndexQueryHandler extends SubgraphQueryHandler<
    Index,
    IndexListQuery,
    IndexesQuery,
    Index_Filter,
    IndexesQueryVariables
> {
    // validateFilter(filter: IndexSubscriptionListQueryFilter) {}

    convertToSubgraphFilter(
        filter: NonNullable<IndexListQuery["filter"]>
    ): Index_Filter {
        return filter;
    }

    protected getRelevantAddressesFromFilterCore = (
        filter: IndexListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [filter.token, filter.token_in, filter.token_not_in],
        accounts: [
            filter.publisher,
            filter.publisher_in,
            filter.publisher_not_in,
        ],
    });

    protected getRelevantAddressesFromResultCore = (
        result: Index
    ): RelevantAddressesIntermediate => ({
        tokens: [result.token],
        accounts: [result.publisher],
    });

    mapFromSubgraphResponse = (response: IndexesQuery): Index[] =>
        response.indexes.map((x) => ({
            ...x,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
            indexCreatedEvent: x.indexCreatedEvent.id,
            publisher: x.publisher.id,
            token: x.token.id,
        }));

    requestDocument = IndexesDocument;
}
