import {
    BlockNumber,
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import {
    Account_Filter,
    Account_OrderBy,
    InputMaybe,
    Scalars,
} from "../../schema.generated";

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

export type AccountListQuery = SubgraphListQuery<
    AccountListQueryFilter,
    Account_OrderBy
>;

export interface AccountListQueryFilter {
    createdAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    createdAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    createdAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    createdAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    isSuperApp?: InputMaybe<Scalars["Boolean"]>;
    updatedAtBlockNumber?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtBlockNumber_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtBlockNumber_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_gte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_in?: InputMaybe<Array<Scalars["BigInt"]>>;
    updatedAtTimestamp_lt?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_lte?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not?: InputMaybe<Scalars["BigInt"]>;
    updatedAtTimestamp_not_in?: InputMaybe<Array<Scalars["BigInt"]>>;
}

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
