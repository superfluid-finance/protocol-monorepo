import { ethers } from "hardhat";
import { toBN } from "../test/helpers/helpers";
import maticAddresses from "../config/matic.json";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import {
    getIndexes,
    getSubscriptions,
} from "../test/queries/dataIntegrityQueries";
import { IDataIntegrityIndex, IDataIntegritySubscription } from "./interfaces";
import { chainIdToData } from "./maps";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import request, { gql } from "graphql-request";
import { IMeta } from "../test/interfaces";

const subgraphRequest = async <T>(
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

const getMostRecentIndexedBlockNumber = async (subgraphEndpoint: string) => {
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
async function getAllResults<T>(
    query: string,
    endpoint: string,
    blockNumber: number,
    resultsPerPage: number,
    counter: number
): Promise<T[]> {
    const initialResults = await subgraphRequest<{ response: T[] }>(
        query,
        endpoint,
        {
            blockNumber,
            first: resultsPerPage,
            skip: resultsPerPage * counter,
        }
    );

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }

    return [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            blockNumber,
            resultsPerPage,
            counter + 1
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

    const indexes = await getAllResults<IDataIntegrityIndex>(
        getIndexes,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        0
    );
    const subscriptions = await getAllResults<IDataIntegritySubscription>(
        getSubscriptions,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        0
    );

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

    const chunkedIndexPromises = chunkPromises(indexPromises, 100);
    const chunkedSubscriptionPromises = chunkPromises(
        subscriptionPromises,
        100
    );
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
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
