import { ethers } from "hardhat";
import _ from "lodash";
import { toBN } from "../../test/helpers/helpers";
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
    idaEventToQueryMap,
    cfaEventToQueryMap,
} from "./dataIntegrityQueries";
import {
    CFAEvent,
    DataIntegrityAccountTokenSnapshot,
    DataIntegrityIndex,
    DataIntegrityStream,
    DataIntegritySubscription,
    DataIntegrityTokenStatistic,
    OnChainCFAEvents,
    OnChainIDAEvents,
} from "./interfaces";
import { chainIdToData } from "../maps";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { ISuperToken } from "../../typechain";
import { calculateAvailableBalance } from "../../../sdk-core/src/utils";
import { IIndexSubscription } from "../../../sdk-core/src/interfaces";
import {
    chunkData,
    getMostRecentIndexedBlockNumber,
    keys,
    printProgress,
    printTestOutcome,
    QueryHelper,
    querySubgraphAndValidateEvents,
} from "../dataIntegrity/helperFunctions";

// currently set to 1 due to limitation with node-fetch
// https://github.com/node-fetch/node-fetch/issues/449
const DEFAULT_CHUNK_LENGTH = 1;
const MAX_RESULTS_PER_PAGE = 1000; // a limit imposed by subgraph

// Reference to the invariants tested:
// https://github.com/superfluid-finance/protocol-monorepo/wiki/Subgraph-Data-Integrity-Invariants-Reference

async function main() {
    let netFlowRateSum = toBN(0);
    let subscriptionUnitsSum = toBN(0);
    let indexUnitsSum = toBN(0);
    let onChainCFAEvents: OnChainCFAEvents = {
        FlowOperatorUpdated: { events: [], groupedEvents: {} },
        FlowUpdated: { events: [], groupedEvents: {} },
    };
    let onChainIDAEvents: OnChainIDAEvents = {
        IndexCreated: { events: [], groupedEvents: {} },
        IndexDistributionClaimed: { events: [], groupedEvents: {} },
        IndexUpdated: { events: [], groupedEvents: {} },
        IndexSubscribed: { events: [], groupedEvents: {} },
        IndexUnitsUpdated: { events: [], groupedEvents: {} },
        IndexUnsubscribed: { events: [], groupedEvents: {} },
        SubscriptionApproved: { events: [], groupedEvents: {} },
        SubscriptionDistributionClaimed: { events: [], groupedEvents: {} },
        SubscriptionRevoked: { events: [], groupedEvents: {} },
        SubscriptionUnitsUpdated: { events: [], groupedEvents: {} },
    };
    let errorCounts = {
        a0: 0,
        a1a: 0,
        a1b: 0,
        a2a: 0,
        a2b: 0,
        g0: 0,
        g3: 0,
        g4: 0,
    };

    const network = await ethers.provider.getNetwork();
    const chainId = network.chainId;
    const chainIdData = chainIdToData.get(chainId);

    if (chainIdData == null) {
        throw new Error("chainId " + chainId + " is not a supported chainId.");
    }

    const currentBlockNumber = await getMostRecentIndexedBlockNumber(
        chainIdData.subgraphAPIEndpoint
    );
    const queryHelper = new QueryHelper(
        chainIdData.subgraphAPIEndpoint,
        currentBlockNumber,
        MAX_RESULTS_PER_PAGE
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

    const flowUpdatedEventFilter = cfaV1.filters.FlowUpdated();
    const flowUpdatedEvents = await cfaV1.queryFilter(
        flowUpdatedEventFilter,
        addresses.hostStartBlock
    );
    const groupedFlowUpdatedEvents = _.groupBy(
        flowUpdatedEvents,
        (x) => x.transactionHash.toLowerCase() + "-" + x.logIndex
    );
    const flowOperatorUpdatedEventFilter = cfaV1.filters.FlowOperatorUpdated();
    const flowOperatorUpdatedEvents = await cfaV1.queryFilter(
        flowOperatorUpdatedEventFilter,
        addresses.hostStartBlock
    );
    const groupedFlowOperatorUpdatedEvents = _.groupBy(
        flowOperatorUpdatedEvents,
        (x) => x.transactionHash.toLowerCase() + "-" + x.logIndex
    );
    onChainCFAEvents[CFAEvent.FlowOperatorUpdated] = {
        events: flowOperatorUpdatedEvents,
        groupedEvents: groupedFlowOperatorUpdatedEvents,
    };
    onChainCFAEvents[CFAEvent.FlowUpdated] = {
        events: flowUpdatedEvents,
        groupedEvents: groupedFlowUpdatedEvents,
    };

    console.log(flowUpdatedEvents.length, "FlowUpdated events queried.");
    console.log(
        flowOperatorUpdatedEvents.length,
        "FlowOperatorUpdated events queried."
    );

    // query and set all the ida events in our onChainEvents object
    await Promise.all(
        keys(onChainIDAEvents).map(async (x) => {
            console.log(`\nQuerying ${x} events...`);
            const idaEventName = x;
            const eventsFilter = idaV1.filters[idaEventName]() as any;
            const events = await idaV1.queryFilter(
                eventsFilter,
                addresses.hostStartBlock
            );
            const groupedEvents = _.groupBy(
                events,
                (x) => x.transactionHash.toLowerCase() + "-" + x.logIndex
            );
            onChainIDAEvents[idaEventName] = { events, groupedEvents };
            console.log(events.length, `${idaEventName} events queried.`);
        })
    );

    console.log("\nCFA/IDA Event Entities Validation Starting (G.3)...");

    console.log("CFA Events Validation Starting...");
    let cfaErrors = 0;
    await Promise.all(
        keys(onChainCFAEvents).map(async (x) => {
            const query = cfaEventToQueryMap.get(x);
            if (!query) {
                throw new Error("No query: invalid CFA event.");
            }
            cfaErrors += await querySubgraphAndValidateEvents({
                queryHelper,
                query,
                onChainCFAEvents,
                eventName: x,
            });
        })
    )

    console.log("\nIDA Events Validation Starting...");
    let idaErrors = 0;
    await Promise.all(
        keys(onChainIDAEvents).map(async (x) => {
            const query = idaEventToQueryMap.get(x);
            if (!query) {
                throw new Error("No query: invalid IDA event.");
            }
            idaErrors += await querySubgraphAndValidateEvents({
                queryHelper,
                query,
                onChainIDAEvents,
                eventName: x,
            });
        })
    );

    // sum CFA/IDA errors
    errorCounts.g3 += cfaErrors + idaErrors;
    printTestOutcome(
        errorCounts.g3 === 0,
        "\nSuccess (G.3): CFA/IDA Events Data Matching",
        "\nFailure (G.3): CFA/IDA Events Data Mismatch"
    );

    console.log(
        "\nSubgraph HOL & Aggregate Entities Data Integrity Tests Starting..."
    );

    console.log("\nQuerying all streams via the Subgraph...");

    // This gets all of the current streams (flow rate > 0)
    const streams = await queryHelper.getAllResults<DataIntegrityStream>({
        query: getCurrentStreams,
        isUpdatedAt: false,
    });

    console.log("\nQuerying all account token snapshots via the Subgraph...");
    // This gets account token snapshots of all accounts that have
    // ever interacted with the Super protocol.
    const accountTokenSnapshots =
        await queryHelper.getAllResults<DataIntegrityAccountTokenSnapshot>({
            query: getAccountTokenSnapshots,
            isUpdatedAt: true,
        });

    console.log("\nQuerying all indexes via the Subgraph...");
    // Gets all indexes ever created
    const indexes = await queryHelper.getAllResults<DataIntegrityIndex>({
        query: getIndexes,
        isUpdatedAt: false,
    });

    console.log("\nQuerying all subscriptions via the Subgraph...");
    // Gets all subscriptions ever created
    const subscriptions =
        await queryHelper.getAllResults<DataIntegritySubscription>({
            query: getSubscriptions,
            isUpdatedAt: false,
        });

    console.log("\nQuerying all tokenStatistics via the Subgraph...");
    // Gets all subscriptions ever created
    const tokenStatistics =
        await queryHelper.getAllResults<DataIntegrityTokenStatistic>({
            query: getTokenStatistics,
            isUpdatedAt: true,
        });

    console.log(
        "\nData Cleaning: Filtering out duplicate HOL & Aggregate entities..."
    );

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
    // TODO: Maybe we want to verify those without underlying too in the
    // future
    const uniqueTokenStatistics = _.uniqBy(tokenStatistics, (x) => x.id).filter(
        (x) => x.token.underlyingAddress !== ethers.constants.AddressZero
    );
    console.log(
        `There are ${uniqueTokenStatistics.length} unique tokenStatistics with an underlying address
        out of ${tokenStatistics.length} total tokenStatistics.`
    );

    console.log(
        "\nValidating integrity of subgraph data for Higher Order Level and Aggregate Entities"
    );

    // Account Level Invariant: A.1.a CFA Flow Data is matching
    console.log("Flow Tests Starting (A.1.a)...");
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
                        { blockTag: currentBlockNumber }
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
                        errorCounts.a1a++;
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
    printTestOutcome(
        errorCounts.a1a === 0,
        "Success (A.1.a): Flow data matching",
        "Failure (A.1.a): Flow data mismatch"
    );

    // Account Level Invariant: A.1.b CFA Net Flow's are matching
    console.log("ATS Net Flow Tests Starting (A.1.b)...");
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
                        errorCounts.a1b++;
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
    printTestOutcome(
        errorCounts.a1b === 0,
        "Success (A.1.b): Net Flow matching",
        "Failure (A.1.b): Net Flow mismatch"
    );

    // Global Invariant: G.1 sum of CFA total netflows should equal 0
    printTestOutcome(
        netFlowRateSum.eq(toBN(0)),
        "Success (G.1): Net flow sum === 0",
        `Failure (G.1): Net flow sum (${netFlowRateSum.toString()}) !== 0`
    );

    // Account Level Invariant: A.0 User RTB === subgraph calculated balance
    console.log(
        "Account Level RTB Invariant: User RTB === Subgraph calculated balance Test Starting... (A.0)"
    );
    const uniqueTokens = _.uniqBy(
        uniqueAccountTokenSnapshots,
        (x) => x.token.id
    ).map((x) => x.token.id);
    let tokenContracts: { [tokenAddress: string]: ISuperToken } = {};
    for (let i = 0; i < uniqueTokens.length; i++) {
        tokenContracts[uniqueTokens[i]] = new ethers.Contract(
            uniqueTokens[i],
            superTokenABI,
            ethers.provider
        ) as ISuperToken;
    }

    for (let i = 0; i < chunkedUniqueAccountTokenSnapshots.length; i++) {
        await Promise.all(
            // gotta chunk this so it works
            chunkedUniqueAccountTokenSnapshots[i].map(async (x, _j) => {
                try {
                    // does this once for each token
                    const tokenContract = tokenContracts[x.token.id];

                    // does this for each ATS
                    const [realtimeBalance] =
                        await tokenContract.realtimeBalanceOfNow(x.account.id, {
                            blockTag: currentBlockNumber,
                        });

                    // get user's subscriptions
                    // TODO: can groupBy tokenId and subscriber earlier for optimization here
                    const userIndexSubscriptions = uniqueSubscriptions.filter(
                        (z) =>
                            z.index.token.id === x.token.id &&
                            z.subscriber.id === x.account.id
                    );

                    // calculate the available balance based on balanceUntilUpdatedAt
                    // as well as indexValue
                    const calculatedAvailableBalance =
                        calculateAvailableBalance({
                            currentBalance: x.balanceUntilUpdatedAt,
                            netFlowRate: x.totalNetFlowRate,
                            currentTimestamp: currentTimestamp.toString(),
                            updatedAtTimestamp: x.updatedAtTimestamp!,

                            // explicit cast for ease of use
                            indexSubscriptions:
                                userIndexSubscriptions as unknown as IIndexSubscription[],
                        });

                    if (!realtimeBalance.eq(calculatedAvailableBalance)) {
                        errorCounts.a0++;
                        throw new Error(
                            `Realtime balance: ${realtimeBalance.toString()} (on-chain)
                                !== ${calculatedAvailableBalance.toString()} (calculated w/ subgraph data)`
                        );
                    }
                } catch (err) {
                    console.error(err);
                }
            })
        );

        printProgress(
            i,
            chunkedUniqueAccountTokenSnapshots.length,
            "User RTB === calculated balance"
        );
    }

    printTestOutcome(
        errorCounts.a0 === 0,
        "Success (A.0): User RTB === Subgraph calculated balance",
        "Failure (A.0): User RTB === Subgraph calculated balance mismatch"
    );

    // Account Level Invariant: A.2.a Validate IDA indexes data
    // Creates promises to validate account level IDA index data
    // AND
    // Global Invariant (G.2): sum of subscriber units === sum of index totalUnitsApproved + index totalUnitsPending
    console.log("Index Tests Starting (A.2.a, G.2)...");
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
                        errorCounts.a2a++;
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
                        errorCounts.a2a++;
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

                    // Global Invariant: G.2 IDA Sum IndexSubscription Units === Index total units
                    const totalSubscriptionUnits = uniqueSubscriptions
                        .filter((x) => x.index.id === index.id)
                        .map((x) => toBN(x.units))
                        .reduce((x, y) => x.add(y), toBN(0));
                    const indexTotalUnits =
                        totalUnitsApproved.add(totalUnitsPending);
                    subscriptionUnitsSum.add(totalSubscriptionUnits);
                    indexUnitsSum.add(indexTotalUnits);
                } catch (err) {
                    console.error("Error: ", err);
                }
            })
        );
    }
    printTestOutcome(
        errorCounts.a2a === 0,
        "Success (A.2.a): Index data matching",
        "Failure (A.2.a): Index data mismatch"
    );

    // G.2 Invariant Results
    printTestOutcome(
        subscriptionUnitsSum.eq(indexUnitsSum),
        "Success (G.2): total subscriber units === total index units",
        `Failure (G.2): Total subscription units !== total index units. \n
        Subscription Units Sum: ${subscriptionUnitsSum.toString()} \n
        Index Units Sum: ${indexUnitsSum.toString()}`
    );

    // Account Level Invariant: A.2.b Validate IDA subscriptions data
    // Creates promises to validate account level IDA subscriptions data
    console.log("Subscription Tests Starting (A.2.b)...");
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
                            { blockTag: currentBlockNumber }
                        );

                    // subscription may have been deleted, but it's ok if units === 0
                    if (!exist && units.gt(toBN(0))) {
                        errorCounts.a2b++;
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
                        errorCounts.a2b++;
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

    printTestOutcome(
        errorCounts.a2b === 0,
        "Success (A.2.b): Subscription data matching",
        "Failure (A.2.b): Subscription data mismatch"
    );

    console.log("Token Global Invariants Tests Starting (G.0, G.4)");
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
                    let aum = await tokenContract.balanceOf(x.id);
                    const superTokenDecimals =
                        await superTokenContract.decimals();
                    const tokenDecimals = await tokenContract.decimals();
                    const decimalsDifference =
                        superTokenDecimals - tokenDecimals;
                    const formattedAUM =
                        decimalsDifference > 0
                            ? ethers.utils.parseUnits(
                                  aum.toString(),
                                  decimalsDifference
                              )
                            : aum;

                    const getTokenNameAndLog = async () => {
                        const name = await tokenContract.name();
                        console.log(
                            `TOKEN (${x.token.underlyingAddress}): ${name}`
                        );
                    };

                    if (!toBN(x.totalSupply).eq(totalSupply)) {
                        await getTokenNameAndLog();
                        errorCounts.g4++;
                        throw new Error(
                            `Failure: Subgraph total supply (${
                                x.totalSupply
                            }) !== on-chain total supply (${totalSupply.toString()})`
                        );
                    }

                    // Global Invariant: G.0 Total Supply === SuperToken AUM
                    if (toBN(x.totalSupply).eq(formattedAUM) === false) {
                        await getTokenNameAndLog();
                        errorCounts.g0++;
                        throw new Error(
                            `Failure (G.0): Subgraph Total Supply (${
                                x.totalSupply
                            }) !== SuperToken AUM (${formattedAUM.toString()})`
                        );
                    }
                } catch (err) {
                    console.error(err);
                }
            })
        );
    }
    printTestOutcome(
        errorCounts.g0 === 0,
        "Success (G.0): Total Supply === SuperToken AUM",
        "Failure (G.0): Total Supply === SuperToken AUM mismatch"
    );
    printTestOutcome(
        errorCounts.g4 === 0,
        "Success (G.4): Subgraph Total Supply === On-Chain Total Supply",
        "Failure (G.4): Subgraph Total Supply === On-Chain Total Supply mismatch"
    );
}

main()
    .then(() => process.exit(0))
    .catch((error) => {
        console.error(error);
        process.exit(1);
    });
