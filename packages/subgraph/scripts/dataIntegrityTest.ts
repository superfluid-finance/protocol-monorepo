import { ethers } from "hardhat";
import _ from "lodash";
import { toBN } from "../test/helpers/helpers";
import maticAddresses from "../config/matic.json";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import {
    getIndexes,
    getStreams,
    getSubscriptions,
} from "../test/queries/dataIntegrityQueries";
import {
    IBaseEntity,
    IDataIntegrityAccountTokenSnapshot,
    IDataIntegrityIndex,
    IDataIntegrityStream,
    IDataIntegritySubscription,
} from "./interfaces";
import { chainIdToData } from "./maps";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import request, { gql } from "graphql-request";
import { IMeta } from "../test/interfaces";

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

export const getMostRecentIndexedBlockNumber = async (subgraphEndpoint: string) => {
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

/**
 * @dev Chunks the promises as we don't want to exhaust CPU.
 * e.g. trying to do a promise for 5,000 items at once.
 */
function chunkPromises(promises: Promise<void>[], chunkLength: number) {
    const chunksLength = Math.ceil(promises.length / chunkLength);
    const batches = Array.apply(null, Array(chunksLength)).map((_x, i) =>
        promises.slice(i * chunkLength, chunkLength * (i + 1))
    );
    return batches;
}

/**
 * @dev Gets all the results from the graph, we need this function
 * due to the 1,000 item limitation imposed by the
 */
async function getAllResults<T extends IBaseEntity>(
    query: string,
    endpoint: string,
    blockNumber: number,
    resultsPerPage: number,
    createdAtTimestamp: number = 0
): Promise<T[]> {
    const initialResults = await subgraphRequest<{ response: T[] }>(
        query,
        endpoint,
        {
            blockNumber,
            first: resultsPerPage,
            createdAt: createdAtTimestamp,
        }
    );

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }
    let newCreatedAtTimestamp =
        initialResults.response[initialResults.response.length - 1]
            .createdAtTimestamp;

    return [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            blockNumber,
            resultsPerPage,
            Number(newCreatedAtTimestamp)
        )) as T[]),
    ];
}

async function main() {
    const network = await ethers.provider.getNetwork();
    const chainId = network.chainId;
    const chainIdData = chainIdToData.get(chainId);
    if (chainIdData == null) {
        throw new Error("chainId " + chainId + " is not a supported chainId.");
    }
    // Give the Indexer 150 block cushion
    const currentBlockNumber = await getMostRecentIndexedBlockNumber(
        chainIdData.subgraphAPIEndpoint
    );
    console.log(
        "Executing Subgraph Data Integrity Test on " +
            chainIdData.name +
            " network."
    );
    console.log("Current block number used to query: ", currentBlockNumber);

    const cfaV1 = (await ethers.getContractAt(
        cfaABI,
        maticAddresses.cfaAddress
    )) as ConstantFlowAgreementV1;
    const idaV1 = (await ethers.getContractAt(
        idaABI,
        maticAddresses.idaAddress
    )) as InstantDistributionAgreementV1;

    // NOTE: this validates net flow rate, but we should aim to
    // also validate the last updated timestamp
    // this requires getting the most recent flowUpdated timestamp of a particular flow
    // and comparing this with the getAccountFlowInfo data instead of getNetFlow data
    async function validateNetFlowRate(
        ats: IDataIntegrityAccountTokenSnapshot
    ) {
        const netFlowRate = await cfaV1.getNetFlow(
            ats.token.id,
            ats.account.id
        );
        const netFlowRateShouldMatch = netFlowRate.eq(
            toBN(ats.totalNetFlowRate)
        );
        if (!netFlowRateShouldMatch) {
            throw new Error(
                "Values don't match. \n Subgraph Net Flow Rate: " +
                    ats.totalNetFlowRate +
                    "\n Contract Data Net Flow Rate: " +
                    netFlowRate.toString()
            );
        }
    }

    const streams = await getAllResults<IDataIntegrityStream>(
        getStreams,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000
    );

    // NOTE: this gets all streams, including one's that have been deleted
    // and restarted again => we must only get the one's that are active
    // which just means where flowRate > 0
    const uniqueStreams = _.uniqBy(
        streams,
        (x) => x.createdAtTimestamp + x.sender.id + x.receiver.id + x.token.id
    );
    const accountTokenSnapshots = _.uniqBy(
        uniqueStreams
            .map((x) => [
                ...x.sender.accountTokenSnapshots,
                ...x.receiver.accountTokenSnapshots,
            ])
            .flat(),
        (x) => x.account.id + x.token.id
    );

    const indexes = await getAllResults<IDataIntegrityIndex>(
        getIndexes,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000
    );
    const subscriptions = await getAllResults<IDataIntegritySubscription>(
        getSubscriptions,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000
    );

    // Account Level Invariant: Validate CFA Data of Current Streams
    const streamPromises = uniqueStreams.map(async (x) => {
        const stream = x;
        try {
            const token = ethers.utils.getAddress(stream.token.id);
            const sender = ethers.utils.getAddress(stream.sender.id);
            const receiver = ethers.utils.getAddress(stream.receiver.id);
            const [updatedAtTimestamp, flowRate] = await cfaV1.getFlow(
                token,
                sender,
                receiver
            );
            const updatedAtShouldMatch = updatedAtTimestamp.eq(
                toBN(stream.updatedAtTimestamp)
            );
            const flowRateShouldMatch = flowRate.eq(
                toBN(stream.currentFlowRate)
            );
            const compareStream = {
                updatedAtTimestamp: stream.updatedAtTimestamp,
                currentFlowRate: stream.currentFlowRate,
            };
            if (!updatedAtShouldMatch || !flowRateShouldMatch) {
                throw new Error(
                    "Values don't match. \n Subgraph Stream: " +
                        JSON.stringify(compareStream) +
                        "\n Contract Data \n Updated At Timestamp: " +
                        updatedAtTimestamp.toString() +
                        " \n Flow Rate: " +
                        flowRate.toString()
                );
            }
        } catch (err) {
            console.error("Error: ", err);
        }
    });

    const accountTokenSnapshotPromises = accountTokenSnapshots.map(async (x) =>
        validateNetFlowRate(x)
    );

    // Account Level Invariant: Validate IDA Data of Indexes
    const indexPromises = indexes.map(async (x) => {
        const index = x;
        try {
            const superToken = ethers.utils.getAddress(index.token.id);
            const publisher = ethers.utils.getAddress(index.publisher.id);
            const indexId = Number(index.indexId);
            const [, indexValue, totalUnitsApproved, totalUnitsPending] =
                await idaV1.getIndex(superToken, publisher, indexId, {
                    blockTag: currentBlockNumber,
                });
            const indexValueShouldMatch = toBN(index.indexValue).eq(indexValue);
            const totalUnitsApprovedShouldMatch = toBN(
                index.totalUnitsApproved
            ).eq(totalUnitsApproved);
            const totalUnitsPendingShouldMatch = toBN(
                index.totalUnitsPending
            ).eq(totalUnitsPending);
            const compareIndex = {
                indexValue: index.indexValue,
                totalUnitsApproved: index.totalUnitsApproved,
                totalUnitsPending: index.totalUnitsPending,
            };
            if (
                !indexValueShouldMatch ||
                !totalUnitsApprovedShouldMatch ||
                !totalUnitsPendingShouldMatch
            ) {
                throw new Error(
                    "Values don't match. \n Subgraph Index: " +
                        JSON.stringify(compareIndex) +
                        "\n Contract Data \n Index Value: " +
                        indexValue.toString() +
                        " \n Approved Units: " +
                        totalUnitsApproved.toString() +
                        " \n Pending Units: " +
                        totalUnitsPending.toString()
                );
            }
        } catch (err) {
            console.error("Error: ", err);
        }
    });

    // Account Level Invariant: Validate IDA Data of Subscriptions
    const subscriptionPromises = subscriptions.map(async (x) => {
        const subscription = x;
        try {
            const superToken = ethers.utils.getAddress(
                subscription.index.token.id
            );
            const publisher = ethers.utils.getAddress(
                subscription.index.publisher.id
            );
            const subscriber = ethers.utils.getAddress(
                subscription.subscriber.id
            );
            const indexId = Number(subscription.index.indexId);
            const [, approved, units, pendingDistribution] =
                await idaV1.getSubscription(
                    superToken,
                    publisher,
                    indexId,
                    subscriber,
                    { blockTag: currentBlockNumber }
                );
            const expectedPendingDistribution = subscription.approved
                ? toBN(0)
                : toBN(subscription.units).mul(
                      toBN(subscription.index.indexValue).sub(
                          toBN(subscription.indexValueUntilUpdatedAt)
                      )
                  );
            const approvedShouldMatch = approved === subscription.approved;
            const unitsShouldMatch = toBN(subscription.units).eq(units);
            const pendingDistributionShouldMatch =
                expectedPendingDistribution.eq(pendingDistribution);
            const compareSubscription = {
                approved: subscription.approved,
                units: subscription.units,
                pendingDistribution: expectedPendingDistribution.toString(),
            };

            if (
                !approvedShouldMatch ||
                !unitsShouldMatch ||
                !pendingDistributionShouldMatch
            ) {
                throw new Error(
                    "Values don't match. \n Subgraph Subscription: " +
                        JSON.stringify(compareSubscription) +
                        "\n Contract Data \n Approved: " +
                        approved +
                        " \n Units: " +
                        units.toString() +
                        " \n Pending Units: " +
                        pendingDistribution.toString()
                );
            }
        } catch (error) {
            console.error("Error: ", error);
        }
    });

    // General TODOS:
    // Clean this file up, add more comments so it's more maintainable, use the SDK-core for the queries.

    // ACCOUNT LEVEL TODOS
    // TODO: Balance Data should match - RTB + Claimable === Subgraph Calculated Balance
    
    // GLOBAL LEVEL TODOS
    // TODO: Validate Total Supply of SuperToken (contract) === Total Supply of SuperToken (subgraph) === sum of all accounts RTB 
    // TODO: CFA Total netflow === 0
    // TODO: Total Subscriber units sum === Total Publisher Index Units
    // TODO: SuperTokens w/ Underlying Token => Underlying Token Total Supply >= sum RTB of SuperToken
    // TODO: Subgraph FlowUpdatedEvents length === on chain FlowUpdated events length AND properties are matching
    // (can apply to other interested events events)
    const chunkedStreamPromises = chunkPromises(streamPromises, 100);
    const chunkedIndexPromises = chunkPromises(indexPromises, 100);
    const chunkedSubscriptionPromises = chunkPromises(
        subscriptionPromises,
        100
    );
    const chunkedATSPromises = chunkPromises(accountTokenSnapshotPromises, 100);
    console.log("Stream Tests Starting...");
    console.log("Validating " + streamPromises.length + " streams.");
    for (let i = 0; i < chunkedStreamPromises.length; i++) {
        await Promise.all(chunkedStreamPromises[i]);
    }
    console.log("Stream Tests Successful!");
    console.log("Index Tests Starting...");
    console.log("Validating " + indexPromises.length + " indexes.");
    for (let i = 0; i < chunkedIndexPromises.length; i++) {
        await Promise.all(chunkedIndexPromises[i]);
    }
    console.log("Index Tests Successful!");
    console.log("Subscription Tests Starting...");
    console.log(
        "Validating " + subscriptionPromises.length + " subscriptions."
    );
    for (let i = 0; i < chunkedSubscriptionPromises.length; i++) {
        await Promise.all(chunkedSubscriptionPromises[i]);
    }
    console.log("Subscription Tests Successful!");
    console.log("Account Token Snapshot Tests Starting...");
    console.log(
        "Validating " +
            accountTokenSnapshotPromises.length +
            " account token snapshots."
    );
    for (let i = 0; i < chunkedATSPromises.length; i++) {
        await Promise.all(chunkedATSPromises[i]);
    }
    console.log("Account Token Snapshot Tests Successful!");
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
