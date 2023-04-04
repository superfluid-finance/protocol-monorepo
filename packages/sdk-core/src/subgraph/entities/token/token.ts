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
    isNativeAssetSuperToken: boolean;
    isSuperToken: boolean;
    name: string;
    symbol: string;
    underlyingAddress: Address;
}

export type TokenListQuery = SubgraphListQuery<Token_Filter, Token_OrderBy>;

export class TokenQueryHandler extends SubgraphQueryHandler<
    Token,
    TokenListQuery,
    TokensQuery,
    TokensQueryVariables
> {
    getAddressFieldKeysFromFilter = (): {
        accountKeys: (keyof Token_Filter)[];
        tokenKeys: (keyof Token_Filter)[];
    } => ({
        accountKeys: [],
        tokenKeys: ["id", "underlyingToken", "underlyingAddress"],
    });

    getRelevantAddressesFromResultCore = (
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
