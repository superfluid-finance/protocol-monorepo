import request, { gql } from "graphql-request";
import _ from "lodash";
import { ethers } from "hardhat";
import { IMeta } from "../test/interfaces";
import { chainIdToData } from "./maps";
import { toBN } from "../test/helpers/helpers";
import CoinGecko from "coingecko-api";
/**
 * NOTE: Run this file using `npx hardhat run scripts/getLiquidations.ts`.
 *
 */

interface IAgreementLiquidatedByEvent {
    readonly id: string;
    readonly transactionHash: string;
    readonly timestamp: string;
    readonly name: string;
    readonly blockNumber: string;
    readonly token: string;
    readonly liquidatorAccount: string;
    readonly agreementClass: string;
    readonly agreementId: string;
    readonly penaltyAccount: string;
    readonly bondAccount: string;
    readonly rewardAmount: string;
    readonly bailoutAmount: string;
}

interface ISuperToken {
    readonly id: string;
    readonly underlyingToken: {
        readonly id: string;
        readonly name: string;
        readonly symbol: string;
    };
}

interface IStream {
    readonly createdAtTimestamp: string;
    readonly flowUpdatedEvents: {
        readonly flowRate: string;
        readonly receiver: string;
        readonly sender: string;
        readonly timestamp: string;
        readonly token: string;
        readonly totalAmountStreamedUntilTimestamp: string;
        readonly transactionHash: string;
        readonly type: number;
    }[];
}

const CoinGeckoClient = new CoinGecko();

const DAY_IN_SECS = 60 * 60 * 24;
const WEEK_IN_SECS = DAY_IN_SECS * 7;

const MIN_DOLLAR_AMOUNT = 5;

export const subgraphRequest = async <T>(
    query: string,
    subgraphEndpoint: string,
    variables?: { [key: string]: any }
): Promise<T> => {
    try {
        const response = await request<T>(subgraphEndpoint, query, variables);
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};

export const getMostRecentIndexedBlockNumber = async (
    subgraphEndpoint: string
) => {
    const query = gql`
        query {
            _meta {
                block {
                    number
                }
            }
        }
    `;
    const data = await subgraphRequest<IMeta>(query, subgraphEndpoint);
    if (!data) return 0;

    return data._meta.block.number;
};

async function getAllResults<T extends { timestamp: string }>(
    query: string,
    endpoint: string,
    resultsPerPage: number,
    earliestTimestamp: number,
    mostRecentTimestamp: number,
    block: number
): Promise<T[]> {
    const initialResults = await subgraphRequest<{ response: T[] }>(
        query,
        endpoint,
        {
            first: resultsPerPage,
            lt_timestamp: mostRecentTimestamp,
            gt_timestamp: earliestTimestamp,
            block,
        }
    );

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }
    let newTimestamp =
        initialResults.response[initialResults.response.length - 1].timestamp;
    const data = [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            resultsPerPage,
            Number(newTimestamp),
            mostRecentTimestamp,
            block
        )) as T[]),
    ];
    console.log("total results retrieved: ", data.length);
    return data;
}

const liquidatedByQuery = gql`
    query getLiquidatedByEvents(
        $lt_timestamp: Int!
        $gt_timestamp: Int!
        $first: Int!
        $block: Int!
    ) {
        response: agreementLiquidatedByEvents(
            block: { number: $block }
            where: {
                timestamp_lte: $lt_timestamp
                timestamp_gte: $gt_timestamp
            }
            first: $first
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            name
            blockNumber
            token
            liquidatorAccount
            agreementClass
            agreementId
            penaltyAccount
            bondAccount
            rewardAmount
            bailoutAmount
        }
    }
`;

const tokensQuery = gql`
    query getTokens($id_in: [ID!], $block: Int!) {
        response: tokens(
            block: { number: $block }
            where: { underlyingToken_not: null, id_in: $id_in }
        ) {
            id
            underlyingToken {
                id
                name
                symbol
            }
        }
    }
`;

const liquidatedStreamsQuery = gql`
    query getStreams(
        $sender_in: [String!]
        $createdAtTimestamp_gte: BigInt!
        $block: Int!
    ) {
        response: streams(
            block: { number: $block }
            where: {
                sender_in: $sender_in
                createdAtTimestamp_gte: $createdAtTimestamp_gte
                currentFlowRate: 0
            }
        ) {
            createdAtTimestamp
            flowUpdatedEvents(
                orderBy: timestamp
                orderDirection: desc
                first: 1
            ) {
                flowRate
                receiver
                sender
                timestamp
                token
                totalAmountStreamedUntilTimestamp
                transactionHash
                type
            }
        }
    }
`;

(async () => {
    const chainId = 137;
    const chainIdData = chainIdToData.get(chainId);
    if (chainIdData == null) {
        throw new Error("chainId " + chainId + " is not a supported chainId.");
    }

    // get the most recent indexed block for executing all subgraph queries based off this block
    const recentBlock = await getMostRecentIndexedBlockNumber(
        chainIdData.subgraphAPIEndpoint
    );
    const provider = new ethers.providers.JsonRpcProvider(
        "https://polygon-rpc.com/"
    );
    const block = await provider.getBlock(recentBlock);

    // get the most recent timestamp (given the most recent block from provider)
    const mostRecentBlockTimestamp = block.timestamp;
    const monthInSeconds = DAY_IN_SECS * 30;

    // get the earliest timestamp we will query by
    const earliestTimestamp = mostRecentBlockTimestamp - monthInSeconds;

    // get all the AgreementLiquidatedByEvents in the last month
    const agreementLiquidatedByEvents =
        await getAllResults<IAgreementLiquidatedByEvent>(
            liquidatedByQuery,
            chainIdData.subgraphAPIEndpoint,
            1000,
            earliestTimestamp,
            mostRecentBlockTimestamp,
            block.number
        );

    const uniqueLiquidatedByEvents = _.uniqBy(
        agreementLiquidatedByEvents,
        (x) => x.id
    );

    // get coingecko's list of tokens
    let coingeckoCoinList: {
        data: { id: string; symbol: string; name: string }[];
    } = await CoinGeckoClient.coins.list();

    // get liquidated tokens
    const liquidatedTokens = _.uniqBy(
        uniqueLiquidatedByEvents,
        (x) => x.token
    ).map((x) => x.token);

    // get more details about the liquidated tokens
    const superTokensResponse = await subgraphRequest<{
        response: ISuperToken[];
    }>(tokensQuery, chainIdData.subgraphAPIEndpoint, {
        id_in: liquidatedTokens,
        block: block.number,
    });

    // Get dictionaries for optimizations
    const superTokenSymbolToBoolDict = superTokensResponse.response.reduce(
        (acc, obj) => {
            return { ...acc, [obj.underlyingToken.symbol.toLowerCase()]: true };
        },
        {} as { [symbol: string]: boolean }
    );

    const superTokenSymbolToIdDict = superTokensResponse.response.reduce(
        (acc, obj) => {
            const { id, underlyingToken } = obj;
            return { ...acc, [underlyingToken.symbol.toLowerCase()]: id };
        },
        {} as { [symbol: string]: string }
    );

    // filter coingecko coin list to only include liquidated tokens
    const filteredCoinList = coingeckoCoinList.data.filter(
        (x) => superTokenSymbolToBoolDict[x.symbol.toLowerCase()]
    );

    // get raw price data of liquidated tokens
    const pricesResponse = await CoinGeckoClient.simple.price({
        ids: filteredCoinList.map((x) => x.id),
        vs_currencies: ["usd"],
    });

    // create dict of [superTokenId]: price
    const tokenPrices: { [superToken: string]: string } = filteredCoinList
        .map((x) => {
            const superToken = superTokenSymbolToIdDict[x.symbol.toLowerCase()];
            return {
                superToken,
                price: pricesResponse.data[x.id].usd,
            };
        })
        .reduce((acc, obj) => {
            let { superToken, price } = obj;
            return { ...acc, [superToken as unknown as string]: price };
        }, {});

    // get filtered agreement liquidated by events where the minimum amount is greater than MIN_DOLLAR_AMOUNT (5)
    const filteredAgreementLiquidatedByEvents = uniqueLiquidatedByEvents.filter(
        (x) =>
            Number(ethers.utils.formatUnits(x.rewardAmount)) *
                Number(tokenPrices[x.token]) >=
            MIN_DOLLAR_AMOUNT
    );

    const filteredPenaltyAccountToTimestampsDict =
        filteredAgreementLiquidatedByEvents.reduce((acc, obj) => {
            const { transactionHash, penaltyAccount, token } = obj;
            return {
                ...acc,
                [penaltyAccount + "-" + token]: [
                    ...(acc[penaltyAccount + "-" + token] || []),
                    transactionHash,
                ],
            };
        }, {} as { [penaltyTokenAccount: string]: string[] });

    // get addresses of the liquidated users
    const liquidatedIndividuals = filteredAgreementLiquidatedByEvents.map(
        (x) => x.penaltyAccount
    );

    // do a query of the possible liquidated streams
    const possibleLiquidatedStreams = await subgraphRequest<{
        response: IStream[];
    }>(liquidatedStreamsQuery, chainIdData.subgraphAPIEndpoint, {
        sender_in: liquidatedIndividuals,
        createdAtTimestamp_gte: earliestTimestamp,
        block: block.number,
    });

    // filter possible liquidated streams <= a week old
    const weekOldPossiblyLiquidatedStreams = possibleLiquidatedStreams.response
        .filter(
            (x) =>
                Number(x.flowUpdatedEvents[0].timestamp) -
                    Number(x.createdAtTimestamp) <=
                WEEK_IN_SECS
        )
        .map((x) => ({
            createdAtTimestamp: x.createdAtTimestamp,
            sender: x.flowUpdatedEvents[0].sender,
            receiver: x.flowUpdatedEvents[0].receiver,
            token: x.flowUpdatedEvents[0].token,
            totalAmountStreamedUntilTimestamp:
                x.flowUpdatedEvents[0].totalAmountStreamedUntilTimestamp,
            transactionHash: x.flowUpdatedEvents[0].transactionHash,
            terminatedAtTimestamp: x.flowUpdatedEvents[0].timestamp,
        }))
        // last check to ensure that the stream closure occurred at the same time as a liquidation
        // it is not 100% proof because the user could've possibly closed their own stream with the
        // same token at the exact moment they got liquidated
        .filter((x) =>
            (
                filteredPenaltyAccountToTimestampsDict[
                    x.sender + "-" + x.token
                ] || []
            ).includes(x.transactionHash)
        );
    console.log(
        "Total Streams Liquidated (1 week or less + > $5 deposit lost): ",
        weekOldPossiblyLiquidatedStreams.length
    );
})();
