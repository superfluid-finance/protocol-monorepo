import {
    Address,
    BlockNumber,
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import {
    InputMaybe,
    Scalars,
    Token_Filter,
    Token_OrderBy,
} from "../../schema.generated";

import {
    TokensDocument,
    TokensQuery,
    TokensQueryVariables,
} from "./tokens.generated";

export interface Token {
    createdAtBlockNumber: BlockNumber;
    createdAtTimestamp: Timestamp;
    decimals: number;
    id: Address;
    isListed: boolean;
    isSuperToken: boolean;
    name: string;
    symbol: string;
    underlyingAddress: Address;
}

export type TokenListQuery = SubgraphListQuery<
    TokenListQueryFilter,
    Token_OrderBy
>;

export interface TokenListQueryFilter {
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
    decimals?: InputMaybe<Scalars["Int"]>;
    decimals_gt?: InputMaybe<Scalars["Int"]>;
    decimals_gte?: InputMaybe<Scalars["Int"]>;
    decimals_in?: InputMaybe<Array<Scalars["Int"]>>;
    decimals_lt?: InputMaybe<Scalars["Int"]>;
    decimals_lte?: InputMaybe<Scalars["Int"]>;
    decimals_not?: InputMaybe<Scalars["Int"]>;
    decimals_not_in?: InputMaybe<Array<Scalars["Int"]>>;
    isListed?: InputMaybe<Scalars["Boolean"]>;
    isListed_in?: InputMaybe<Array<Scalars["Boolean"]>>;
    isListed_not?: InputMaybe<Scalars["Boolean"]>;
    isListed_not_in?: InputMaybe<Array<Scalars["Boolean"]>>;
    isSuperToken?: InputMaybe<Scalars["Boolean"]>;
    isSuperToken_in?: InputMaybe<Array<Scalars["Boolean"]>>;
    isSuperToken_not?: InputMaybe<Scalars["Boolean"]>;
    isSuperToken_not_in?: InputMaybe<Array<Scalars["Boolean"]>>;
    name?: InputMaybe<Scalars["String"]>;
    name_contains?: InputMaybe<Scalars["String"]>;
    name_ends_with?: InputMaybe<Scalars["String"]>;
    name_gt?: InputMaybe<Scalars["String"]>;
    name_gte?: InputMaybe<Scalars["String"]>;
    name_in?: InputMaybe<Array<Scalars["String"]>>;
    name_lt?: InputMaybe<Scalars["String"]>;
    name_lte?: InputMaybe<Scalars["String"]>;
    name_not?: InputMaybe<Scalars["String"]>;
    name_not_contains?: InputMaybe<Scalars["String"]>;
    name_not_ends_with?: InputMaybe<Scalars["String"]>;
    name_not_in?: InputMaybe<Array<Scalars["String"]>>;
    name_not_starts_with?: InputMaybe<Scalars["String"]>;
    name_starts_with?: InputMaybe<Scalars["String"]>;
    symbol?: InputMaybe<Scalars["String"]>;
    symbol_contains?: InputMaybe<Scalars["String"]>;
    symbol_ends_with?: InputMaybe<Scalars["String"]>;
    symbol_gt?: InputMaybe<Scalars["String"]>;
    symbol_gte?: InputMaybe<Scalars["String"]>;
    symbol_in?: InputMaybe<Array<Scalars["String"]>>;
    symbol_lt?: InputMaybe<Scalars["String"]>;
    symbol_lte?: InputMaybe<Scalars["String"]>;
    symbol_not?: InputMaybe<Scalars["String"]>;
    symbol_not_contains?: InputMaybe<Scalars["String"]>;
    symbol_not_ends_with?: InputMaybe<Scalars["String"]>;
    symbol_not_in?: InputMaybe<Array<Scalars["String"]>>;
    symbol_not_starts_with?: InputMaybe<Scalars["String"]>;
    symbol_starts_with?: InputMaybe<Scalars["String"]>;
    underlyingAddress?: InputMaybe<Scalars["Bytes"]>;
    underlyingAddress_in?: InputMaybe<Array<Scalars["Bytes"]>>;
    underlyingAddress_not_in?: InputMaybe<Array<Scalars["Bytes"]>>;
    underlyingToken?: InputMaybe<Scalars["String"]>;
    underlyingToken_in?: InputMaybe<Array<Scalars["String"]>>;
    underlyingToken_not_in?: InputMaybe<Array<Scalars["String"]>>;
}

export class TokenQueryHandler extends SubgraphQueryHandler<
    Token,
    TokenListQuery,
    TokensQuery,
    Token_Filter,
    TokensQueryVariables
> {
    convertToSubgraphFilter = (filter: TokenListQueryFilter): Token_Filter =>
        filter;

    protected getRelevantAddressesFromFilterCore = (
        filter: TokenListQuery["filter"]
    ): RelevantAddressesIntermediate => ({
        tokens: [
            filter.underlyingToken,
            filter.underlyingToken_in,
            filter.underlyingToken_not_in,
        ],
        accounts: [],
    });

    protected getRelevantAddressesFromResultCore = (
        result: Token
    ): RelevantAddressesIntermediate => ({
        tokens: [result.underlyingAddress, result.id],
        accounts: [],
    });

    mapFromSubgraphResponse = (response: TokensQuery): Token[] =>
        response.tokens.map((x) => ({
            ...x,
            createdAtBlockNumber: Number(x.createdAtBlockNumber),
            createdAtTimestamp: Number(x.createdAtTimestamp),
        }));

    requestDocument = TokensDocument;
}
