import {
    Address,
    BigNumber,
    BlockNumber,
    SubgraphId,
    Timestamp,
} from "../../mappedSubgraphTypes";
import { Index_Filter, Index_OrderBy } from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    IndexesDocument,
    IndexesQuery,
    IndexesQueryVariables,
} from "./indexes.generated";

export interface Index {
    id: SubgraphId;
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    updatedAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
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

export type IndexListQuery = SubgraphListQuery<Index_Filter, Index_OrderBy>;

export class IndexQueryHandler extends SubgraphQueryHandler<
    Index,
    IndexListQuery,
    IndexesQuery,
    IndexesQueryVariables
> {
    protected getRelevantAddressesFromFilterCore = (
        filter: Index_Filter
    ): RelevantAddressesIntermediate => ({
        tokens: [
            filter.token,
            filter.token_in,
            filter.token_not,
            filter.token_not_in,
        ],
        accounts: [
            filter.publisher,
            filter.publisher_in,
            filter.publisher_not,
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
