import { BlockNumber, Timestamp } from "../../mappedSubgraphTypes";
import { Account_Filter, Account_OrderBy } from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

import {
    AccountsDocument,
    AccountsQuery,
    AccountsQueryVariables,
} from "./accounts.generated";

export interface Account {
    id: string;
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    updatedAtBlockNumber: BlockNumber;
    updatedAtTimestamp: Timestamp;
    isSuperApp: boolean;
}

export type AccountListQuery = SubgraphListQuery<
    Account_Filter,
    Account_OrderBy
>;

export class AccountQueryHandler extends SubgraphQueryHandler<
    Account,
    AccountListQuery,
    AccountsQuery,
    AccountsQueryVariables
> {
    getRelevantAddressesFromFilterCore = (
        filter: Account_Filter
    ): RelevantAddressesIntermediate => ({
        accounts: [filter.id, filter.id_in, filter.id_not, filter.id_not_in],
        tokens: [],
    });

    getRelevantAddressesFromResultCore = (
        result: Account
    ): RelevantAddressesIntermediate => ({
        accounts: [result.id],
        tokens: [],
    });

    mapFromSubgraphResponse = (response: AccountsQuery): Account[] =>
        response.accounts.map((x) => ({
            ...x,
            createdAtTimestamp: Number(x.createdAtTimestamp),
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            updatedAtTimestamp: Number(x.updatedAtTimestamp),
            updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
        }));

    requestDocument = AccountsDocument;
}
