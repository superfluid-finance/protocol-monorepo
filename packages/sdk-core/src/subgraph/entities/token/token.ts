import { Address, BlockNumber, Timestamp } from "../../mappedSubgraphTypes";
import { Token_Filter, Token_OrderBy } from "../../schema.generated";
import {
    RelevantAddressesIntermediate,
    SubgraphListQuery,
    SubgraphQueryHandler,
} from "../../subgraphQueryHandler";

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

export type TokenListQuery = SubgraphListQuery<Token_Filter, Token_OrderBy>;

export class TokenQueryHandler extends SubgraphQueryHandler<
    Token,
    TokenListQuery,
    TokensQuery,
    TokensQueryVariables
> {
    protected getRelevantAddressesFromFilterCore = (
        filter: Token_Filter
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
