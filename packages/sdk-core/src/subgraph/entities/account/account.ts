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
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof Account_Filter)[];
        tokenKeys: (keyof Account_Filter)[];
    } => ({
        accountKeys: ["id"],
        tokenKeys: [],
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
