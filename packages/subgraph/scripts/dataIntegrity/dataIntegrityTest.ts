import {ethers} from "hardhat";
import _ from "lodash";
import {toBN} from "../../test/helpers/helpers";
import maticAddresses from "../../config/matic.json";
import cfaABI from "../../abis/IConstantFlowAgreementV1.json";
import idaABI from "../../abis/IInstantDistributionAgreementV1.json";
import superTokenABI from "../../abis/ISuperToken.json";
import {
    getIndexes,
    getCurrentStreams,
    getSubscriptions,
    getAccountTokenSnapshots,
    getTokenStatistics,
} from "./dataIntegrityQueries";
import {
    IBaseEntity,
    IDataIntegrityAccountTokenSnapshot,
    IDataIntegrityIndex,
    IDataIntegrityStream,
    IDataIntegritySubscription,
    IDataIntegrityTokenStatistic,
} from "../interfaces";
import {chainIdToData} from "../maps";
import {ConstantFlowAgreementV1} from "../../typechain/ConstantFlowAgreementV1";
import {InstantDistributionAgreementV1} from "../../typechain/InstantDistributionAgreementV1";
import request, {gql} from "graphql-request";
import {IMeta} from "../../test/interfaces";
import {ISuperToken} from "../../typechain";
import {calculateAvailableBalance} from "../../../sdk-core/src/utils";
import {IIndexSubscription} from "../../../sdk-core/src/interfaces";
import {BigNumber} from "ethers";

export const subgraphRequest = async <T>(
    query: string,
    subgraphEndpoint: string,
    variables?: {[key: string]: any}
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

/**
 * @dev Chunks the promises as we don't want to exhaust CPU.
 * e.g. trying to do a promise for 5,000 items at once.
 */
function chunkPromises(promises: Promise<void>[], chunkLength: number) {
    const chunksLength =
        promises.length < chunkLength
            ? promises.length
            : Math.ceil(promises.length / chunkLength);
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
    isUpdatedAt: boolean,
    timestamp: number = 0
): Promise<T[]> {
    const baseQuery = {blockNumber, first: resultsPerPage};
    const initialResults = await subgraphRequest<{response: T[]}>(
        query,
        endpoint,
        isUpdatedAt
            ? {
                  ...baseQuery,
                  updatedAt: timestamp,
              }
            : {
                  ...baseQuery,
                  createdAt: timestamp,
              }
    );

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }

    let newTimestamp = isUpdatedAt
        ? initialResults.response[initialResults.response.length - 1]
              .updatedAtTimestamp
        : initialResults.response[initialResults.response.length - 1]
              .createdAtTimestamp;

    return [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            blockNumber,
            resultsPerPage,
            isUpdatedAt,
            Number(newTimestamp)
        )) as T[]),
    ];
}

async function main() {
    let netFlowRateSum = toBN(0);
    let tokenGroupedRTBSums: {[tokenAddress: string]: BigNumber} = {};
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
    const block = await ethers.provider.getBlock(currentBlockNumber);
    const currentTimestamp = block.timestamp;
    console.log(
        "Executing Subgraph Data Integrity Test on " +
            chainIdData.name +
            " network."
    );
    console.log("Current block number used to query: ", currentBlockNumber);
    console.log("Current timestamp: ", currentTimestamp);

    const cfaV1 = (await ethers.getContractAt(
        cfaABI,
        maticAddresses.cfaAddress
    )) as ConstantFlowAgreementV1;
    const idaV1 = (await ethers.getContractAt(
        idaABI,
        maticAddresses.idaAddress
    )) as InstantDistributionAgreementV1;

    /**
     * Validates the net flow rate of the user - compares it to the ATS entity.
     * Also adds netFlowRate to netFlowRateSum.
     * @param ats
     */
    async function validateAccountLevelNetFlowRate(
        ats: IDataIntegrityAccountTokenSnapshot
    ) {
        const netFlowRate = await cfaV1.getNetFlow(
            ats.token.id,
            ats.account.id,
            {blockTag: currentBlockNumber}
        );
        const netFlowRateShouldMatch = netFlowRate.eq(
            toBN(ats.totalNetFlowRate)
        );
        netFlowRateSum = netFlowRateSum.add(netFlowRate);
        if (!netFlowRateShouldMatch) {
            throw new Error(
                "Values don't match. \n Subgraph Net Flow Rate: " +
                    ats.totalNetFlowRate +
                    "\n Contract Data Net Flow Rate: " +
                    netFlowRate.toString()
            );
        }
    }

    console.log("Querying all streams via the Subgraph...");
    // This gets all of the current streams (flow rate > 0)
    const streams = await getAllResults<IDataIntegrityStream>(
        getCurrentStreams,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        false
    );

    console.log("Querying all account token snapshots via the Subgraph...");
    // This gets account token snapshots of all accounts that have
    // ever interacted with the Super protocol.
    const accountTokenSnapshots =
        await getAllResults<IDataIntegrityAccountTokenSnapshot>(
            getAccountTokenSnapshots,
            chainIdData.subgraphAPIEndpoint,
            currentBlockNumber,
            1000,
            true
        );

    console.log("Querying all indexes via the Subgraph...");
    // Gets all indexes ever created
    const indexes = await getAllResults<IDataIntegrityIndex>(
        getIndexes,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        false
    );

    console.log("Querying all subscriptions via the Subgraph...");
    // Gets all subscriptions ever created
    const subscriptions = await getAllResults<IDataIntegritySubscription>(
        getSubscriptions,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        false
    );

    console.log("Querying all tokenStatistics via the Subgraph...");
    // Gets all subscriptions ever created
    const tokenStatistics = await getAllResults<IDataIntegrityTokenStatistic>(
        getTokenStatistics,
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        1000,
        true
    );

    console.log("Filtering out duplicate entities...");
    const uniqueStreams = _.uniqBy(
        streams,
        (x) => x.createdAtTimestamp + x.sender.id + x.receiver.id + x.token.id
    );
    console.log(
        `There are ${uniqueStreams.length} unique streams out of ${streams.length} total streams.`
    );

    const uniqueAccountTokenSnapshots = _.uniqBy(
        accountTokenSnapshots,
        (x) => x.id
    );
    console.log(
        `There are ${uniqueAccountTokenSnapshots.length} unique accountTokenSnapshots
        out of ${accountTokenSnapshots.length} total accountTokenSnapshots.`
    );

    const uniqueIndexes = _.uniqBy(indexes, (x) => x.id);
    console.log(
        `There are ${uniqueIndexes.length} unique indexes
        out of ${indexes.length} total indexes.`
    );

    const uniqueSubscriptions = _.uniqBy(subscriptions, (x) => x.id);
    console.log(
        `There are ${uniqueSubscriptions.length} unique subscriptions
        out of ${subscriptions.length} total subscriptions.`
    );

    // NOTE: we only care about super tokens with underlying for
    // our data integrity tests
    const uniqueTokenStatistics = _.uniqBy(tokenStatistics, (x) => x.id).filter(
        (x) => x.token.underlyingAddress !== ethers.constants.AddressZero
    );
    console.log(
        `There are ${uniqueTokenStatistics.length} unique tokenStatistics
        out of ${tokenStatistics.length} total tokenStatistics.`
    );

    // Account Level Invariant: validate CFA current streams data
    // Create promises to validate account level CFA stream data
    const streamPromises = uniqueStreams.map(async (x) => {
        const stream = x;
        try {
            const [updatedAtTimestamp, flowRate] = await cfaV1.getFlow(
                ethers.utils.getAddress(stream.token.id),
                ethers.utils.getAddress(stream.sender.id),
                ethers.utils.getAddress(stream.receiver.id),
                {blockTag: currentBlockNumber}
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

    // Create promises to validate account level CFA stream data
    // AND
    // sum net flow rates to validate global invariant: CFA total netflow === 0
    const netFlowRatePromises = uniqueAccountTokenSnapshots.map(async (x) =>
        validateAccountLevelNetFlowRate(x)
    );

    // Create token balance promises to validate account level RTB === subgraph
    // calculated balance
    // AND
    // sum RTB of supertoken's with underlying to check global invariant:
    // underlying Token supply >= sum RTB of token
    const tokenGroupedATSEntities = _.groupBy(
        uniqueAccountTokenSnapshots,
        (x) => x.token.id
    );
    const tokenGroupedATSArray = Object.entries(tokenGroupedATSEntities);
    const tokenBalancePromises = tokenGroupedATSArray.map(async (x) => {
        // does this once for each token
        const tokenContract = (await ethers.getContractAt(
            superTokenABI,
            x[0]
        )) as ISuperToken;

        const promises = x[1].map(async (y) => {
            // does this for each ATS
            const [realtimeBalance] = await tokenContract.realtimeBalanceOfNow(
                y.account.id,
                {
                    blockTag: currentBlockNumber,
                }
            );

            // get user's subscriptions
            // TODO: can groupBy tokenId and subscriber earlier for optimization here
            const userIndexSubscriptions = uniqueSubscriptions.filter(
                (z) =>
                    z.index.token.id === x[0] &&
                    z.subscriber.id === y.account.id
            );

            // calculate the available balance based on balanceUntilUpdatedAt
            // as well as indexValue
            const calculatedAvailableBalance = calculateAvailableBalance({
                currentBalance: y.balanceUntilUpdatedAt,
                netFlowRate: y.totalNetFlowRate,
                currentTimestamp: currentTimestamp.toString(),
                updatedAtTimestamp: y.updatedAtTimestamp!,

                // explicit cast for ease of use
                indexSubscriptions:
                    userIndexSubscriptions as unknown as IIndexSubscription[],
            });

            if (!realtimeBalance.eq(calculatedAvailableBalance)) {
                throw new Error(
                    `Realtime balance: ${realtimeBalance.toString()} !== ${calculatedAvailableBalance.toString()}`
                );
            }

            // only sum RTB for comparison of supertokens with underlying
            if (
                ethers.utils.getAddress(y.token.underlyingAddress) !==
                ethers.constants.AddressZero
            ) {
                tokenGroupedRTBSums[y.token.underlyingAddress] =
                    tokenGroupedRTBSums[y.token.underlyingAddress].add(
                        realtimeBalance
                    );
            }
        });

        const chunkedBalancePromises = chunkPromises(promises, 100);
        for (let i = 0; i < chunkedBalancePromises.length; i++) {
            await Promise.all(chunkedBalancePromises[i]);
        }
    });

    // Account Level Invariant: Validate IDA indexes data
    // Creates promises to validate account level IDA index data
    // AND
    // global invariant: sum of subscriber units === sum of index totalUnitsApproved + index totalUnitsPending
    const indexPromises = uniqueIndexes.map(async (x) => {
        const index = x;
        try {
            const superToken = ethers.utils.getAddress(index.token.id);
            const publisher = ethers.utils.getAddress(index.publisher.id);
            const indexId = Number(index.indexId);
            const [exist, indexValue, totalUnitsApproved, totalUnitsPending] =
                await idaV1.getIndex(superToken, publisher, indexId, {
                    blockTag: currentBlockNumber,
                });

            if (!exist) {
                throw new Error("This index doesn't exist.");
            }

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

            // validate global level invariant regarding total index and total subscription units
            const subscriptionUnitsSum = uniqueSubscriptions
                .filter((x) => x.index.id === index.id)
                .map((x) => toBN(x.units))
                .reduce((x, y) => x.add(y), toBN(0));
            const indexTotalUnits = totalUnitsApproved.add(totalUnitsPending);

            if (!subscriptionUnitsSum.eq(indexTotalUnits)) {
                throw new Error(`Global invariant failed,
                    total subscription units !== total index units. \n
                    Subscription Units Sum: ${subscriptionUnitsSum.toString()} \n
                    Index Units Sum: ${indexTotalUnits.toString()}`);
            }
        } catch (err) {
            console.error("Error: ", err);
        }
    });

    // Account Level Invariant: Validate IDA subscriptions data
    // Creates promises to validate account level IDA subscriptions data
    const subscriptionPromises = uniqueSubscriptions.map(async (x) => {
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
            const [exist, approved, units, pendingDistribution] =
                await idaV1.getSubscription(
                    superToken,
                    publisher,
                    indexId,
                    subscriber,
                    {blockTag: currentBlockNumber}
                );

            if (!exist) {
                throw new Error("This subscription doesn't exist.");
            }

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

    const totalSupplyPromises = uniqueTokenStatistics.map(async (x) => {
        const superTokenContract = (await ethers.getContractAt(
            superTokenABI,
            x.id
        )) as ISuperToken;
        const tokenContract = (await ethers.getContractAt(
            superTokenABI,
            x.token.underlyingAddress
        )) as ISuperToken;
        const totalSupply = await superTokenContract.totalSupply();
        const aum = await tokenContract.balanceOf(x.id);
        const tokenSumRTB = tokenGroupedRTBSums[x.id];
        if (!toBN(x.totalSupply).eq(totalSupply)) {
            throw new Error(
                `Subgraph total supply: ${
                    x.totalSupply
                } !== on chain total supply: ${totalSupply.toString()}`
            );
        }
        if (!toBN(x.totalSupply).eq(aum)) {
            throw new Error(
                `Global invariant failed: subgraph total supply === SuperToken AUM`
            );
        }
        if (!aum.gte(tokenSumRTB)) {
            throw new Error(
                `Global invariant failed: SuperToken AUM >= sum RTB of token`
            );
        }
    });

    // General TODOS:
    // Clean this file up, add more comments so it's more maintainable.

    // GLOBAL LEVEL TODOS
    // TODO: Subgraph FlowUpdatedEvents length === on chain FlowUpdated events length AND properties are matching
    // (can apply to other interested events events)

    console.log("Stream Tests Starting...");
    console.log("Validating " + streamPromises.length + " streams.");
    const chunkedStreamPromises = chunkPromises(streamPromises, 100);
    for (let i = 0; i < chunkedStreamPromises.length; i++) {
        await Promise.all(chunkedStreamPromises[i]);
    }
    console.log("Stream Tests Successful.");

    console.log("Index Tests Starting...");
    console.log("Validating " + indexPromises.length + " indexes.");
    const chunkedIndexPromises = chunkPromises(indexPromises, 100);
    for (let i = 0; i < chunkedIndexPromises.length; i++) {
        await Promise.all(chunkedIndexPromises[i]);
    }
    console.log("Index Tests Successful.");

    console.log("Subscription Tests Starting...");
    console.log(
        "Validating " + subscriptionPromises.length + " subscriptions."
    );
    const chunkedSubscriptionPromises = chunkPromises(
        subscriptionPromises,
        100
    );
    for (let i = 0; i < chunkedSubscriptionPromises.length; i++) {
        await Promise.all(chunkedSubscriptionPromises[i]);
    }
    console.log("Subscription Tests Successful.");

    console.log("Account Token Snapshot Tests Starting...");
    console.log(
        "Validating " +
            netFlowRatePromises.length +
            " account token snapshot net flow rates."
    );
    const chunkedNetFlowATSPromises = chunkPromises(netFlowRatePromises, 100);
    for (let i = 0; i < chunkedNetFlowATSPromises.length; i++) {
        await Promise.all(chunkedNetFlowATSPromises[i]);
    }
    console.log("Net flow rate validation successful.");

    if (netFlowRateSum.eq(toBN(0))) {
        console.log("'Net flow sum === 0' global invariant successful.");
    } else {
        throw new Error(
            `'Net flow sum: ${netFlowRateSum.toString()} !== 0' global invariant failed.`
        );
    }

    console.log(
        "Validating RTB Invariant: User RTB === Subgraph calculated balance"
    );
    await Promise.all(tokenBalancePromises);

    console.log("Token Statistics Total Supply Tests Starting");
    const chunkedTotalSupplyPromises = chunkPromises(totalSupplyPromises, 100);
    for (let i = 0; i < chunkedTotalSupplyPromises.length; i++) {
        await Promise.all(chunkedTotalSupplyPromises[i]);
    }
    console.log("Token Statistics Total Supply validation successful.");
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
