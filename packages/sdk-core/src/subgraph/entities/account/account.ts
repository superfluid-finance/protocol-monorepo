import {
    BlockNumber,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import { Account_Filter, Account_OrderBy } from "../../schema.generated";

import {
    AccountsDocument,
    AccountsQuery,
    AccountsQueryVariables,
} from "./accounts.generated";

export interface Account {
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    id: string;
    isSuperApp: boolean;
    updatedAtBlockNumber: BlockNumber;
    updatedAtTimestamp: Timestamp;
}

export type AccountOrderBy = Account_OrderBy;

export type AccountListQuery = SubgraphListQuery<
    AccountListQueryFilter,
    AccountOrderBy
>;

export type AccountListQueryFilter = Omit<
    Account_Filter,
    SubgraphFilterOmitFieldList
>;

export class AccountQueryHandler extends SubgraphQueryHandler<
    Account,
    AccountListQuery,
    AccountsQuery,
    Account_Filter,
    AccountsQueryVariables
> {
    convertToSubgraphFilter = (
        filter: AccountListQueryFilter
    ): Account_Filter => filter;

    getRelevantAddressesFromFilterCore = (
        _filter: AccountListQueryFilter
    ): RelevantAddressesIntermediate => ({
        accounts: [],
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
