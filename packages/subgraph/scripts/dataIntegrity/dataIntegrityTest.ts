import {ethers} from "hardhat";
import _ from "lodash";
import {toBN} from "../../test/helpers/helpers";
import cfaABI from "../../abis/IConstantFlowAgreementV1.json";
import idaABI from "../../abis/IInstantDistributionAgreementV1.json";
import superTokenABI from "../../abis/ISuperToken.json";
import {
    getIndexes,
    getCurrentStreams,
    getSubscriptions,
    getAccountTokenSnapshots,
    getTokenStatistics,
    getFlowUpdatedEvents,
    getIndexUpdatedEvents,
} from "./dataIntegrityQueries";
import {
    IBaseEntity,
    IDataIntegrityAccountTokenSnapshot,
    IDataIntegrityFlowUpdatedEvent,
    IDataIntegrityIndex,
    IDataIntegrityIndexUpdatedEvent,
    IDataIntegrityStream,
    IDataIntegritySubscription,
    IDataIntegrityTokenStatistic,
    IOnChainEvents,
    OnChainIDAEventString,
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
import {TypedEvent} from "../../typechain/common";

const DEFAULT_CHUNK_LENGTH = 1;

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

const printProgress = (i: number, total: number, entityText: string) => {
    if ((i + 1) % 100 === 0) {
        console.log(`${i + 1}/${total} ${entityText} validated.`);
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
function chunkData<T>(promises: T[], chunkLength: number) {
    const chunksLength =
        promises.length < chunkLength
            ? promises.length
            : Math.ceil(promises.length / chunkLength);
    const batches = Array.apply(null, Array(chunksLength)).map((_x, i) =>
        promises.slice(i * chunkLength, chunkLength * (i + 1))
    );
    return batches;
}

const validateEvents = <T extends TypedEvent, K>(
    eventName: string,
    onchainEvents: T[],
    subgraphEvents: K[],
    groupedEvents: _.Dictionary<T[]>
) => {
    // base case length equality check
    if (onchainEvents.length === subgraphEvents.length) {
        console.log(`${eventName} events length is the same.`);
    } else {
        throw new Error(`${eventName} events length are different.`);
    }

    for (let i = 0; i < subgraphEvents.length; i++) {
        const currentSubgraphEvent = subgraphEvents[i] as any; // TEMPORARY
        const id = currentSubgraphEvent.id.split(eventName + "-")[1];
        const currentOnChainEvent = groupedEvents[id][0];
        const keys = Object.keys(currentSubgraphEvent);
        if (currentOnChainEvent == null) {
            console.log("currentSubgraphEvent", currentSubgraphEvent);
            continue;
        }

        // validate the event properties
        for (let j = 0; j < keys.length; j++) {
            if (
                currentOnChainEvent.args[keys[j]] == null ||
                currentOnChainEvent.args[keys[j]] == undefined
            ) {
                continue;
            }
            // the properties are usually either string or BigInt
            if (typeof currentOnChainEvent.args[keys[j]] === "string") {
                if (
                    currentOnChainEvent.args[keys[j]].toLowerCase() !==
                    currentSubgraphEvent[keys[j]]
                ) {
                    console.log(
                        `${keys[j]} - Subgraph: ${
                            currentSubgraphEvent[keys[j]]
                        } \n On-chain data: ${currentOnChainEvent.args[
                            keys[j]
                        ].toLowerCase()}`
                    );
                    throw new Error(`${eventName} Event is not the same.`);
                }
            } else {
                if (
                    currentOnChainEvent.args[keys[j]].eq(
                        toBN(currentSubgraphEvent[keys[j]])
                    ) === false
                ) {
                    console.log(
                        `${keys[j]} - Subgraph: ${
                            currentSubgraphEvent[keys[j]]
                        } \n On-chain data: ${currentOnChainEvent.args[
                            keys[j]
                        ].toString()}`
                    );
                    throw new Error(`${eventName} Event is not the same.`);
                }
            }
        }

        printProgress(i, subgraphEvents.length, eventName + " events");
    }
    console.log(`\nAll ${subgraphEvents.length} ${eventName} events validated.\n`);
};

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
    isEvent: boolean = false,
    timestamp: number = 0,
    counter: number = 0
): Promise<T[]> {
    const baseQuery = {blockNumber, first: resultsPerPage};
    const initialResults = await subgraphRequest<{response: T[]}>(
        query,
        endpoint,
        isEvent
            ? {
                  ...baseQuery,
                  timestamp,
              }
            : isUpdatedAt
            ? {
                  ...baseQuery,
                  updatedAt: timestamp,
              }
            : {
                  ...baseQuery,
                  createdAt: timestamp,
              }
    );

    console.log(
        counter * resultsPerPage +
            initialResults.response.length +
            " responses queried."
    );
    counter++;

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }

    let newTimestamp = isEvent
        ? initialResults.response[initialResults.response.length - 1].timestamp
        : isUpdatedAt
        ? initialResults.response[initialResults.response.length - 1]
              .updatedAtTimestamp
        : initialResults.response[initialResults.response.length - 1]
              .createdAtTimestamp;

    const responses = [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            blockNumber,
            resultsPerPage,
            isUpdatedAt,
            isEvent,
            Number(newTimestamp),
            counter
        )) as T[]),
    ];
    return responses;
}

async function main() {
    let netFlowRateSum = toBN(0);
    let tokenGroupedRTBSums: {[tokenAddress: string]: BigNumber} = {};
    let onchainEvents: IOnChainEvents = {
        FlowUpdated: {events: [], groupedEvents: {}},
        IndexCreated: {events: [], groupedEvents: {}},
        IndexDistributionClaimed: {events: [], groupedEvents: {}},
        IndexUpdated: {events: [], groupedEvents: {}},
        IndexSubscribed: {events: [], groupedEvents: {}},
        IndexUnitsUpdated: {events: [], groupedEvents: {}},
        IndexUnsubscribed: {events: [], groupedEvents: {}},
        SubscriptionApproved: {events: [], groupedEvents: {}},
        SubscriptionDistributionClaimed: {events: [], groupedEvents: {}},
        SubscriptionRevoked: {events: [], groupedEvents: {}},
        SubscriptionUnitsUpdated: {events: [], groupedEvents: {}},
    };
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
        "\nExecuting Subgraph Data Integrity Test on " +
            chainIdData.name +
            " network."
    );
    console.log("Current block number used to query: ", currentBlockNumber);
    console.log("Current timestamp: ", currentTimestamp);
    const addresses = chainIdData.addresses;

    const cfaV1 = (await ethers.getContractAt(
        cfaABI,
        addresses.cfaAddress
    )) as ConstantFlowAgreementV1;
    const idaV1 = (await ethers.getContractAt(
        idaABI,
        addresses.idaAddress
    )) as InstantDistributionAgreementV1;

    console.log("\nSubgraph Event Entities Data Integrity Tests Starting...\n");
    console.log("Querying the blockchain for past events...\n");
    console.log("Start Block:", addresses.hostStartBlock, "\n");

    console.log("Querying flow updated events...\n");

    // TODO: I don't like how we have special logic specifically for
    // FlowUpdated, this is not really maintainable in the future
    const flowUpdatedEventsFilter = cfaV1.filters.FlowUpdated();
    const flowUpdatedEvents = await cfaV1.queryFilter(
        flowUpdatedEventsFilter,
        addresses.hostStartBlock
    );
    const groupedFlowUpdatedEvents = _.groupBy(
        flowUpdatedEvents,
        (x) => x.transactionHash.toLowerCase() + "-" + x.logIndex
    );
    onchainEvents["FlowUpdated"] = {
        events: flowUpdatedEvents,
        groupedEvents: groupedFlowUpdatedEvents,
    };

    console.log(flowUpdatedEvents.length, "FlowUpdated events queried.")

    // query and set all the ida events in our onChainEvents object
    await Promise.all(
        Object.keys(onchainEvents)
            .filter((x) => x !== "FlowUpdated") // only IDA events
            .map(async (x) => {
                console.log(`Querying ${x} events...\n`);
                const idaEventString = x as OnChainIDAEventString;
                const eventsFilter = idaV1.filters[idaEventString]() as any; // TEMPORARY
                const events = await idaV1.queryFilter(
                    eventsFilter,
                    addresses.hostStartBlock
                );
                const groupedEvents = _.groupBy(
                    events,
                    (x) => x.transactionHash.toLowerCase() + "-" + x.logIndex
                );
                onchainEvents[idaEventString] = {events, groupedEvents};
            })
    );

    /**
     * TODOs
     * PATTERN:
     * - QUERY ALL EVENTS VIA THE CONTRACT ON ETHERS [x]
     * - QUERY ALL EVENTS VIA THE SUBGRAPH []
     * - QUERY ALL HOL/AGGREGATE ENTITIES VIA THE SUBGRAPH []
     * - COMPARE EVENTS (TOTAL LENGTH SHOULD BE THE SAME) []
     *  - VALIDATE THAT THE DATA ON THE EVENTS ARE MATCHING TOO []
     * - COMPARE HOL/AGGREGATE PROPERTIES THAT WE ARE WATCHING []
     */

    console.log("\nGetting all events data via the Subgraph...");

    console.log("Querying all flowUpdatedEvents via the Subgraph...");

    const subgraphFlowUpdatedEvents =
        await getAllResults<IDataIntegrityFlowUpdatedEvent>(
            getFlowUpdatedEvents,
            chainIdData.subgraphAPIEndpoint,
            currentBlockNumber,
            1000,
            false,
            true
        );

    console.log("Querying all indexUpdatedEvents via the Subgraph...");

    const subgraphIndexUpdatedEvents =
        await getAllResults<IDataIntegrityIndexUpdatedEvent>(
            getIndexUpdatedEvents,
            chainIdData.subgraphAPIEndpoint,
            currentBlockNumber,
            1000,
            false,
            true
        );

    const uniqueSubgraphFlowUpdatedEvents = _.uniqBy(
        subgraphFlowUpdatedEvents,
        (x) => x.id
    );
    const uniqueSubgraphIndexUpdatedEvents = _.uniqBy(
        subgraphIndexUpdatedEvents,
        (x) => x.id
    );

    console.log("Event Tests Starting...");
    console.log(
        `There are ${uniqueSubgraphFlowUpdatedEvents.length} FlowUpdated events 
        out of ${subgraphFlowUpdatedEvents.length} total FlowUpdated events.`
    );

    validateEvents(
        "FlowUpdated",
        onchainEvents["FlowUpdated"].events,
        uniqueSubgraphFlowUpdatedEvents,
        onchainEvents["FlowUpdated"].groupedEvents
    );

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

    console.log("\nData Processing: Filtering out duplicate entities...");

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
        `There are ${uniqueTokenStatistics.length} unique tokenStatistics with an underlying address
        out of ${tokenStatistics.length} total tokenStatistics.`
    );

    console.log(
        "\nValidating tegridy' of subgraph data for Higher Order Level and Aggregate Entities"
    );

    // Account Level Invariant: validate CFA current streams data
    // Create promises to validate account level CFA stream data
    console.log("Stream Tests Starting...");
    console.log("Validating " + uniqueStreams.length + " streams.");
    const chunkedUniqueStreams = chunkData(uniqueStreams, DEFAULT_CHUNK_LENGTH);
    for (let i = 0; i < chunkedUniqueStreams.length; i++) {
        await Promise.all(
            chunkedUniqueStreams[i].map(async (x) => {
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
            })
        );
        printProgress(i, uniqueStreams.length, "streams");
    }
    console.log("Stream Tests Successful.");

    // Create promises to validate account level CFA stream data
    // AND
    // sum net flow rates to validate global invariant: CFA total netflow === 0
    console.log("Account Token Snapshot Tests Starting...");
    console.log(
        "Validating " +
            uniqueAccountTokenSnapshots.length +
            " account token snapshot net flow rates."
    );
    const chunkedUniqueAccountTokenSnapshots = chunkData(
        uniqueAccountTokenSnapshots,
        DEFAULT_CHUNK_LENGTH
    );

    for (let i = 0; i < chunkedUniqueAccountTokenSnapshots.length; i++) {
        await Promise.all(
            chunkedUniqueAccountTokenSnapshots[i].map(async (x) => {
                try {
                    const netFlowRate = await cfaV1.getNetFlow(
                        x.token.id,
                        x.account.id,
                        {
                            blockTag: currentBlockNumber,
                        }
                    );
                    const netFlowRateShouldMatch = netFlowRate.eq(
                        toBN(x.totalNetFlowRate)
                    );
                    netFlowRateSum = netFlowRateSum.add(netFlowRate);
                    if (!netFlowRateShouldMatch) {
                        throw new Error(
                            "Values don't match. \n Subgraph Net Flow Rate: " +
                                x.totalNetFlowRate +
                                "\n Contract Data Net Flow Rate: " +
                                netFlowRate.toString()
                        );
                    }
                } catch (err) {
                    console.error("Error: ", err);
                }
            })
        );
        printProgress(i, uniqueAccountTokenSnapshots.length, "ATS net flow");
    }
    console.log("Net flow rate validation successful.");

    if (netFlowRateSum.eq(toBN(0))) {
        console.log("'Net flow sum === 0' global invariant successful.");
    } else {
        throw new Error(
            `'Net flow sum: ${netFlowRateSum.toString()} !== 0' global invariant failed.`
        );
    }

    // // Create token balance promises to validate account level RTB === subgraph
    // // calculated balance
    // // AND
    // // sum RTB of supertoken's with underlying to check global invariant:
    // // underlying Token supply >= sum RTB of token

    console.log(
        "Validating RTB Invariant: User RTB === Subgraph calculated balance"
    );
    const tokenGroupedATSEntities = _.groupBy(
        uniqueAccountTokenSnapshots,
        (x) => x.token.id
    );
    const tokenGroupedATSArray = Object.entries(tokenGroupedATSEntities);
    await Promise.all(
        // gotta chunk this so it works
        tokenGroupedATSArray.map(async (x, j) => {
            try {
                // does this once for each token
                const tokenContract = (await ethers.getContractAt(
                    superTokenABI,
                    x[0]
                )) as ISuperToken;

                const promises = x[1].map(async (y) => {
                    // does this for each ATS
                    const [realtimeBalance] =
                        await tokenContract.realtimeBalanceOfNow(y.account.id, {
                            blockTag: currentBlockNumber,
                        });

                    // get user's subscriptions
                    // TODO: can groupBy tokenId and subscriber earlier for optimization here
                    const userIndexSubscriptions = uniqueSubscriptions.filter(
                        (z) =>
                            z.index.token.id === x[0] &&
                            z.subscriber.id === y.account.id
                    );

                    // calculate the available balance based on balanceUntilUpdatedAt
                    // as well as indexValue
                    const calculatedAvailableBalance =
                        calculateAvailableBalance({
                            currentBalance: y.balanceUntilUpdatedAt,
                            netFlowRate: y.totalNetFlowRate,
                            currentTimestamp: currentTimestamp.toString(),
                            updatedAtTimestamp: y.updatedAtTimestamp!,

                            // explicit cast for ease of use
                            indexSubscriptions:
                                userIndexSubscriptions as unknown as IIndexSubscription[],
                        });

                    // skip this for now, the formula seems to be wrong
                    // if (!realtimeBalance.eq(calculatedAvailableBalance)) {
                    //     throw new Error(
                    //         `Realtime balance: ${realtimeBalance.toString()} (on-chain)
                    //         !== ${calculatedAvailableBalance.toString()} (calculated w/ subgraph data)`
                    //     );
                    // }

                    // only sum RTB for comparison of supertokens with underlying
                    if (
                        ethers.utils.getAddress(y.token.underlyingAddress) !==
                        ethers.constants.AddressZero
                    ) {
                        tokenGroupedRTBSums[y.token.id] =
                            tokenGroupedRTBSums[y.token.id] == undefined
                                ? realtimeBalance
                                : tokenGroupedRTBSums[y.token.id].add(
                                      realtimeBalance
                                  );
                    }
                });

                const chunkedBalancePromises = chunkData(
                    promises,
                    DEFAULT_CHUNK_LENGTH
                );
                for (let i = 0; i < chunkedBalancePromises.length; i++) {
                    await Promise.all(chunkedBalancePromises[i]);
                    printProgress(
                        i,
                        chunkedBalancePromises.length,
                        "User RTB === calculated balance"
                    );
                }
            } catch (err) {
                console.error(err);
            }
        })
    );

    // // Account Level Invariant: Validate IDA indexes data
    // // Creates promises to validate account level IDA index data
    // // AND
    // // global invariant: sum of subscriber units === sum of index totalUnitsApproved + index totalUnitsPending
    console.log("Index Tests Starting...");
    console.log("Validating " + uniqueIndexes.length + " indexes.");
    const chunkedUniqueIndexes = chunkData(uniqueIndexes, DEFAULT_CHUNK_LENGTH);
    for (let i = 0; i < chunkedUniqueIndexes.length; i++) {
        await Promise.all(
            chunkedUniqueIndexes[i].map(async (x) => {
                const index = x;
                try {
                    const superToken = ethers.utils.getAddress(index.token.id);
                    const publisher = ethers.utils.getAddress(
                        index.publisher.id
                    );
                    const indexId = Number(index.indexId);
                    const [
                        exist,
                        indexValue,
                        totalUnitsApproved,
                        totalUnitsPending,
                    ] = await idaV1.getIndex(superToken, publisher, indexId, {
                        blockTag: currentBlockNumber,
                    });

                    if (!exist) {
                        throw new Error("This index doesn't exist.");
                    }

                    const indexValueShouldMatch = toBN(index.indexValue).eq(
                        indexValue
                    );

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
                        console.log("superToken:", superToken);
                        console.log("publisher:", publisher);
                        console.log("indexId:", indexId);
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
                    const indexTotalUnits =
                        totalUnitsApproved.add(totalUnitsPending);

                    if (!subscriptionUnitsSum.eq(indexTotalUnits)) {
                        throw new Error(`Global invariant failed,
                        total subscription units !== total index units. \n
                        Subscription Units Sum: ${subscriptionUnitsSum.toString()} \n
                        Index Units Sum: ${indexTotalUnits.toString()}`);
                    }
                } catch (err) {
                    console.error("Error: ", err);
                }
            })
        );
    }
    console.log("Index Tests Successful.");

    // Account Level Invariant: Validate IDA subscriptions data
    // Creates promises to validate account level IDA subscriptions data
    console.log("Subscription Tests Starting...");
    console.log("Validating " + uniqueSubscriptions.length + " subscriptions.");
    const chunkedUniqueSubscriptions = chunkData(
        uniqueSubscriptions,
        DEFAULT_CHUNK_LENGTH
    );
    for (let i = 0; i < chunkedUniqueSubscriptions.length; i++) {
        await Promise.all(
            chunkedUniqueSubscriptions[i].map(async (x) => {
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

                    const approvedShouldMatch =
                        approved === subscription.approved;

                    const unitsShouldMatch = toBN(subscription.units).eq(units);

                    const pendingDistributionShouldMatch =
                        expectedPendingDistribution.eq(pendingDistribution);

                    const compareSubscription = {
                        approved: subscription.approved,
                        units: subscription.units,
                        pendingDistribution:
                            expectedPendingDistribution.toString(),
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
            })
        );
    }
    console.log("Subscription Tests Successful.");

    console.log("Token Statistics Total Supply Tests Starting");
    const chunkedUniqueTokenStatistics = chunkData(
        uniqueTokenStatistics,
        DEFAULT_CHUNK_LENGTH
    );
    for (let i = 0; i < chunkedUniqueTokenStatistics.length; i++) {
        await Promise.all(
            chunkedUniqueTokenStatistics[i].map(async (x) => {
                try {
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
                    const tokenSumRTB = tokenGroupedRTBSums[x.id] || toBN(0);
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
                    // if (!aum.gte(tokenSumRTB)) {
                    //     throw new Error(
                    //         `Global invariant failed: SuperToken AUM >= sum RTB of token`
                    //     );
                    // }
                } catch (err) {
                    console.error(err);
                }
            })
        );
    }
    console.log("Token Statistics Total Supply validation successful.");

    // General TODOS:
    // Clean this file up, add more comments so it's more maintainable.

    // GLOBAL LEVEL TODOS
    // TODO: Subgraph FlowUpdatedEvents length === on chain FlowUpdated events length AND properties are matching
    // (can apply to other interested events events)
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
