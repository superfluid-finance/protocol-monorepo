import {
    Address,
    BlockNumber,
    RelevantAddressesIntermediate,
    SubgraphFilterOmitFieldList,
    SubgraphListQuery,
    SubgraphQueryHandler,
    Timestamp,
} from "../../../queryV2";
import { Token_Filter, Token_OrderBy } from "../../schema.generated";

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

export type TokenOrderBy = Token_OrderBy;

export type TokenListQuery = SubgraphListQuery<
    TokenListQueryFilter,
    TokenOrderBy
>;

export type TokenListQueryFilter = Omit<
    Token_Filter,
    SubgraphFilterOmitFieldList
>;

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
        filter: TokenListQueryFilter
    ): RelevantAddressesIntermediate => ({
        tokens: [
            filter.id,
            filter.id_in,
            filter.id_not,
            filter.id_not_in,
            filter.underlyingToken,
            filter.underlyingToken_in,
            filter.underlyingToken_not,
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
