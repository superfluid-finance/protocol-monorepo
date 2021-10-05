import { ethers } from "hardhat";
import { BigNumber, ContractReceipt } from "ethers";
import { BaseProvider } from "@ethersproject/providers";
import { request, gql } from "graphql-request";
import SuperfluidSDK from "@superfluid-finance/js-sdk";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    IAccountTokenSnapshot,
    IFlowUpdatedInitTestData,
    IFlowUpdatedUpdateTestData,
    IIndex,
    IInstantDistributionTestData,
    IMeta,
    IStreamData,
    ISubscriber,
    ITokenStatistic,
    IUpdateIndexData,
} from "../interfaces";
import {
    actionTypeToActiveStreamsDeltaMap,
    actionTypeToClosedStreamsDeltaMap,
    FlowActionType,
} from "./constants";
import { SuperToken } from "../../typechain/SuperToken";
import { ConstantFlowAgreementV1 } from "../../typechain/ConstantFlowAgreementV1";
import { ConstantFlowAgreementV1Helper } from "@superfluid-finance/js-sdk/src/ConstantFlowAgreementV1Helper";

// the resolver address should be consistent as long as you use the
// first account retrieved by hardhat's ethers.getSigners():
// 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266 and the nonce is 0
const RESOLVER_ADDRESS = "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";

/**************************************************************************
 * Test Helper Functions
 *************************************************************************/
/**
 * Before setup function - deploy the framework, get signers, deploy test tokens,
 * initialize framework.
 * @param userAddresses
 * @returns
 */
export const beforeSetup = async (tokenAmount: number) => {
    let names: { [address: string]: string } = {};
    const [Deployer, Alpha, Bravo, Charlie] = (await ethers.getSigners()).map(
        (x) => x.address
    );
    const userAddresses = [Deployer, Alpha, Bravo, Charlie];
    // names[Bob] = "Bob";
    const sf: Framework = new SuperfluidSDK.Framework({
        web3: (global as any).web3,
        version: "test",
        tokens: ["fDAI"],
        resolverAddress: RESOLVER_ADDRESS,
    });

    console.log("\n");
    await sf.initialize();

    // types not properly handling this case
    const dai = await (sf.contracts as any).TestToken.at(
        sf.tokens.fDAI.address
    );
    const daix = sf.tokens.fDAIx;

    console.log(
        "Mint fDAI, approve fDAIx allowance and upgrade fDAI to fDAIx for users..."
    );
    const amount = tokenAmount.toFixed(0);

    for (let i = 0; i < userAddresses.length; i++) {
        const address = userAddresses[i];
        await dai.mint(address, ethers.utils.parseUnits(amount).toString(), {
            from: userAddresses[0],
        });
        await dai.approve(
            daix.address,
            ethers.utils.parseUnits(amount).toString(),
            {
                from: address,
            }
        );
        await daix.upgrade(ethers.utils.parseUnits(amount).toString(), {
            from: address,
        });
    }

    console.log(
        "\n************** Superfluid Framework Setup Complete **************\n"
    );

    return [names, userAddresses, sf, dai, daix];
};

export const monthlyToSecondRate = (monthlyRate: number) => {
    const days = 30;
    const hours = days * 24;
    const minutes = hours * 60;
    const seconds = minutes * 60;
    return Math.round((monthlyRate / seconds) * 10 ** 18);
};

export const getRandomFlowRate = (max: number) =>
    Math.floor(Math.random() * max);

export const getCurrentBlockNumber = async () => {
    const query = gql`
        query {
            _meta {
                block {
                    number
                }
            }
        }
    `;
    const data = await subgraphRequest<IMeta>(query);
    if (!data) return 0;

    return data._meta.block.number;
};

export const subgraphRequest = async <T>(
    query: string,
    variables?: { [key: string]: any }
): Promise<T> => {
    try {
        const response = await request<T>(
            "http://localhost:8000/subgraphs/name/superfluid-test",
            query,
            variables
        );
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};

export const fetchEventAndEnsureExistence = async <T>(
    query: string,
    transactionHash: string,
    queryResultName: string,
    eventName: string
) => {
    const vars = {
        transactionHash,
    };
    const data = await subgraphRequest<{
        [queryResultName: string]: T[];
    }>(query, vars);
    const event = data[queryResultName][0];

    if (!event) {
        throw new Error(eventName + " entity not found.");
    }

    return event;
};

/**
 * To ethers.BigNumber
 */
export function toBN(num: any) {
    return ethers.BigNumber.from(num);
}

function asleep(ms: number) {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

export const waitUntilBlockIndexed = async (txnBlockNumber: number) => {
    let currentBlock: number;
    do {
        currentBlock = await getCurrentBlockNumber();
        await asleep(50);
    } while (txnBlockNumber > currentBlock);
};

export const getCurrentTotalAmountStreamed = (
    streamedSoFar: string,
    currentTime: string,
    lastUpdatedAtTime: string,
    outflowRate: string
) => {
    return toBN(streamedSoFar).add(
        toBN(outflowRate).mul(toBN(currentTime).sub(toBN(lastUpdatedAtTime)))
    );
};

/**************************************************************************
 * Entity ID Getters
 *************************************************************************/

export const getEventId = (receipt: ContractReceipt) => {
    return (
        receipt.transactionHash.toLowerCase() + "-" + receipt.transactionIndex
    );
};

export const getStreamId = (
    sender: string,
    receiver: string,
    token: string,
    revisionIndex: string
) =>
    [
        sender.toLowerCase(),
        receiver.toLowerCase(),
        token.toLowerCase(),
        revisionIndex + ".0",
    ].join("-");

export const getRevisionIndexId = (
    sender: string,
    receiver: string,
    token: string
) =>
    [sender.toLowerCase(), receiver.toLowerCase(), token.toLowerCase()].join(
        "-"
    );

export const getIndexId = (publisher: string, token: string, indexId: string) =>
    [publisher.toLowerCase(), token.toLowerCase(), indexId.toLowerCase()].join(
        "-"
    );

export const getSubscriberId = (
    subscriber: string,
    publisher: string,
    token: string,
    indexId: string
) =>
    [
        subscriber.toLowerCase(),
        publisher.toLowerCase(),
        token.toLowerCase(),
        indexId.toLowerCase(),
    ].join("-");

/**************************************************************************
 * Entity Get or Init Functions
 *************************************************************************/

export const getOrInitRevisionIndex = (
    revisionIndex: { [id: string]: number | undefined },
    revisionIndexId: string
) => {
    return revisionIndex[revisionIndexId] || 0;
};

export const getOrInitStreamData = (
    streamData: { [id: string]: IStreamData | undefined },
    revisionIndex: string,
    streamId: string,
    lastUpdatedAtTimestamp: string
) => {
    const existingStreamData = streamData[streamId];
    if (existingStreamData == null) {
        return {
            id: streamId,
            revisionIndex,
            oldFlowRate: "0",
            streamedUntilUpdatedAt: "0",
            lastUpdatedAtTimestamp,
        };
    }
    return existingStreamData;
};

export const getOrInitAccountTokenSnapshot = (
    accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    },
    accountId: string,
    tokenId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
) => {
    const atsId = accountId + "-" + tokenId;
    const existingATS = accountTokenSnapshots[atsId];
    if (existingATS == null) {
        return {
            id: accountId + "-" + tokenId,
            updatedAtBlock,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalSubscriptions: 0,
            totalApprovedSubscriptions: 0,
            balanceUntilUpdatedAt: "0",
            totalNetFlowRate: "0",
            totalInflowRate: "0",
            totalOutflowRate: "0",
            totalAmountStreamedUntilUpdatedAt: "0",
            totalAmountTransferredUntilUpdatedAt: "0",
            account: { id: accountId },
            token: { id: tokenId },
        };
    }
    return existingATS;
};

export const getOrInitIndex = (
    indexes: { [id: string]: IIndex | undefined },
    indexEntityId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
): IIndex => {
    const existingIndex = indexes[indexEntityId];
    if (existingIndex == null) {
        const [publisher, token, indexId] = indexEntityId.split("-");
        return {
            id: indexEntityId,
            createdAt: updatedAtTimestamp,
            updatedAtBlock: updatedAtBlock,
            updatedAtTimestamp,
            indexId,
            publisher: { id: publisher },
            token: { id: token },
            userData: "0x",
            oldIndexValue: "0",
            newIndexValue: "0",
            totalSubscribers: 0,
            totalUnits: "0",
            totalUnitsApproved: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalUnitsPending: "0",
        };
    }
    return existingIndex;
};

export const getOrInitSubscriber = (
    subscribers: { [id: string]: ISubscriber | undefined },
    subscriberId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
): ISubscriber => {
    const existingSubscriber = subscribers[subscriberId];
    if (existingSubscriber == null) {
        const [subscriber, publisher, token, indexId] = subscriberId.split("-");
        const indexEntityId = getIndexId(publisher, token, indexId);
        return {
            id: subscriberId,
            createdAt: updatedAtTimestamp,
            updatedAtBlock: updatedAtBlock,
            updatedAtTimestamp: updatedAtTimestamp,
            token: { id: token },
            subscriber: { id: subscriber },
            publisher: { id: publisher },
            indexId,
            userData: "0x",
            approved: false,
            units: "0",
            totalAmountReceivedUntilUpdatedAt: "0",
            lastIndexValue: "0",
            index: { id: indexEntityId },
        };
    }
    return existingSubscriber;
};

export const getOrInitTokenStatistics = (
    tokenStatistics: { [id: string]: ITokenStatistic | undefined },
    tokenId: string,
    updatedAtBlock: string,
    updatedAtTimestamp: string
) => {
    const existingTokenStats = tokenStatistics[tokenId];
    if (existingTokenStats == null) {
        return {
            id: tokenId,
            updatedAtBlock,
            updatedAtTimestamp,
            totalNumberOfActiveStreams: 0,
            totalNumberOfClosedStreams: 0,
            totalNumberOfIndexes: 0,
            totalNumberOfActiveIndexes: 0,
            totalSubscriptions: 0,
            totalApprovedSubscriptions: 0,
            totalOutflowRate: "0",
            totalAmountStreamedUntilUpdatedAt: "0",
            totalAmountTransferredUntilUpdatedAt: "0",
            totalAmountDistributedUntilUpdatedAt: "0",
            totalSupply: "0",
            token: { id: tokenId },
        };
    }
    return existingTokenStats;
};

/**
 * Gets/Initializes all data for the FlowUpdated event
 * @param testData
 * @returns
 */
export function getOrInitializeDataForFlowUpdated(
    testData: IFlowUpdatedInitTestData
) {
    const {
        accountTokenSnapshots,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        receiver,
        revisionIndexes,
        sender,
        streamData,
        token,
        tokenStatistics,
    } = testData;

    const revisionIndexId = getRevisionIndexId(sender, receiver, token);
    const tokenId = token.toLowerCase();
    const currentRevisionIndex = getOrInitRevisionIndex(
        revisionIndexes,
        revisionIndexId
    );
    const streamId = getStreamId(
        sender,
        receiver,
        token,
        currentRevisionIndex.toString()
    );
    const pastStreamData = getOrInitStreamData(
        streamData,
        currentRevisionIndex.toString(),
        streamId,
        lastUpdatedAtTimestamp
    );
    const currentSenderATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        sender.toLowerCase(),
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentReceiverATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        receiver.toLowerCase(),
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        tokenId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    return {
        currentSenderATS,
        currentReceiverATS,
        currentTokenStats,
        pastStreamData,
        revisionIndexId,
    };
}

export function getOrInitializeDataForIDA(
    testData: IInstantDistributionTestData
) {
    const {
        accountTokenSnapshots,
        indexes,
        indexId,
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        publisher,
        subscriber,
        subscribers,
        token,
        tokenStatistics,
    } = testData;
    let subscriberAddress = subscriber || "";
    const subscriberEntityId = getSubscriberId(
        subscriberAddress,
        publisher,
        token,
        indexId
    );
    const indexEntityId = getIndexId(publisher, token, indexId);
    const currentIndex = getOrInitIndex(
        indexes,
        indexEntityId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentSubscriber = getOrInitSubscriber(
        subscribers,
        subscriberEntityId,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentPublisherATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        publisher.toLowerCase(),
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    const currentSubscriberATS = getOrInitAccountTokenSnapshot(
        accountTokenSnapshots,
        subscriberAddress.toLowerCase(),
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );

    const currentTokenStats = getOrInitTokenStatistics(
        tokenStatistics,
        token.toLowerCase(),
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp
    );
    return {
        subscriberEntityId,
        indexEntityId,
        currentIndex,
        currentSubscriber,
        currentPublisherATS,
        currentSubscriberATS,
        currentTokenStats,
    };
}

/**************************************************************************
 * Entity Updaters (For expected values)
 *************************************************************************/

export const updateAndReturnStreamData = (
    currentStreamData: IStreamData,
    actionType: FlowActionType,
    oldFlowRate: string,
    lastUpdatedAtTimestamp: string,
    streamedAmountSinceUpdatedAt: BigNumber
) => {
    const revisionIndexDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const revisionIndex = (
        Number(currentStreamData.revisionIndex) + revisionIndexDelta
    ).toString();
    const updatedStreamedUntilUpdatedAt = toBN(
        currentStreamData.streamedUntilUpdatedAt
    ).add(streamedAmountSinceUpdatedAt);
    return {
        ...currentStreamData,
        revisionIndex,
        oldFlowRate,
        streamedUntilUpdatedAt: updatedStreamedUntilUpdatedAt.toString(),
        lastUpdatedAtTimestamp,
    } as IStreamData;
};

export const updateAndReturnIndexData = (
    currentIndex: IIndex,
    updatedIndexData: IUpdateIndexData
) => {
    const {
        userData,
        oldIndexValue,
        newIndexValue,
        totalSubscribersDelta,
        totalUnitsPending,
        totalUnitsApproved,
    } = updatedIndexData;
    const updatedData = {
        ...currentIndex,
        userData: userData == null ? currentIndex.userData : userData,
        oldIndexValue:
            oldIndexValue == null ? currentIndex.oldIndexValue : oldIndexValue,
        newIndexValue:
            newIndexValue == null ? currentIndex.newIndexValue : newIndexValue,
        totalSubscribers:
            currentIndex.totalSubscribers +
            (totalSubscribersDelta == null ? 0 : totalSubscribersDelta),
        totalUnitsPending:
            totalUnitsPending == null
                ? currentIndex.totalUnitsPending
                : totalUnitsPending.toString(),
        totalUnitsApproved:
            totalUnitsApproved == null
                ? currentIndex.totalUnitsApproved
                : totalUnitsApproved.toString(),
    };

    return {
        ...currentIndex,
        ...updatedData,
    };
};

/**
 * Updates ATS entity balance and stream data.
 * @param superToken
 * @param currentATS
 * @param updatedAtBlock
 * @param lastUpdatedAtTimestamp
 * @param actionType
 * @param isSender
 * @param flowRate
 * @param flowRateDelta
 * @returns
 */
export const updateAndReturnATSForCFAData = async (
    superToken: SuperToken,
    currentATS: IAccountTokenSnapshot,
    updatedAtBlock: string,
    lastUpdatedAtTimestamp: string,
    actionType: FlowActionType,
    isSender: boolean,
    flowRate: BigNumber,
    flowRateDelta: BigNumber
): Promise<IAccountTokenSnapshot> => {
    const balanceUntilUpdatedAt = (
        await superToken.balanceOf(currentATS.account.id)
    ).toString();

    // Force casting because they will never be undefined
    const activeStreamsDelta =
        actionTypeToActiveStreamsDeltaMap.get(actionType)!;
    const closedStreamsDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const totalNetFlowRate = isSender
        ? toBN(currentATS.totalNetFlowRate).sub(flowRateDelta).toString()
        : toBN(currentATS.totalNetFlowRate).add(flowRateDelta).toString();
    const inflowRate = toBN(currentATS.totalInflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentATS.totalInflowRate).add(flowRateDelta);
    const outflowRate = toBN(currentATS.totalOutflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentATS.totalOutflowRate).add(flowRateDelta);
    const totalInflowRate =
        isSender === true ? currentATS.totalInflowRate : inflowRate.toString();
    const totalOutflowRate =
        isSender === true
            ? outflowRate.toString()
            : currentATS.totalOutflowRate;
    const totalAmountStreamedUntilUpdatedAt =
        isSender === true
            ? toBN(currentATS.totalAmountStreamedUntilUpdatedAt)
                  .add(
                      toBN(currentATS.totalOutflowRate).mul(
                          toBN(lastUpdatedAtTimestamp).sub(
                              toBN(currentATS.updatedAtTimestamp)
                          )
                      )
                  )
                  .toString()
            : currentATS.totalAmountStreamedUntilUpdatedAt;
    return {
        ...currentATS,
        updatedAtBlock,
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentATS.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentATS.totalNumberOfClosedStreams + closedStreamsDelta,
        balanceUntilUpdatedAt,
        totalNetFlowRate,
        totalInflowRate,
        totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt,
    };
};

/**
 * Updates Token Stats stream data.
 * @param currentTokenStats
 * @param accountTokenSnapshots
 * @param updatedAtBlock
 * @param lastUpdatedAtTimestamp
 * @param actionType
 * @param flowRate
 * @param flowRateDelta
 * @returns
 */
export const updateAndReturnTokenStatsForCFAData = (
    currentTokenStats: ITokenStatistic,
    accountTokenSnapshots: IAccountTokenSnapshot[],
    updatedAtBlock: string,
    lastUpdatedAtTimestamp: string,
    actionType: FlowActionType,
    flowRate: BigNumber,
    flowRateDelta: BigNumber
) => {
    const activeStreamsDelta =
        actionTypeToActiveStreamsDeltaMap.get(actionType)!;
    const closedStreamsDelta =
        actionTypeToClosedStreamsDeltaMap.get(actionType)!;
    const outflowRate = toBN(currentTokenStats.totalOutflowRate)
        .add(flowRateDelta)
        .lt(toBN(0))
        ? flowRate
        : toBN(currentTokenStats.totalOutflowRate).add(flowRateDelta);
    const totalOutflowRate = outflowRate.toString();

    const totalAmountStreamedUntilUpdatedAt = toBN(
        currentTokenStats.totalAmountStreamedUntilUpdatedAt
    )
        .add(
            toBN(currentTokenStats.totalOutflowRate).mul(
                toBN(lastUpdatedAtTimestamp).sub(
                    toBN(currentTokenStats.updatedAtTimestamp)
                )
            )
        )
        .toString();

    // TODO: consider summing all ATS and comparing it with that
    // consider that the ATS updatedAt times are all different.
    const atsSum = accountTokenSnapshots
        .map((x) =>
            getCurrentTotalAmountStreamed(
                x.totalAmountStreamedUntilUpdatedAt,
                lastUpdatedAtTimestamp,
                x.updatedAtTimestamp,
                x.totalOutflowRate
            )
        )
        .reduce((a, b) => a.add(b), toBN(0));
    return {
        ...currentTokenStats,
        updatedAtBlock,
        updatedAtTimestamp: lastUpdatedAtTimestamp,
        totalNumberOfActiveStreams:
            currentTokenStats.totalNumberOfActiveStreams + activeStreamsDelta,
        totalNumberOfClosedStreams:
            currentTokenStats.totalNumberOfClosedStreams + closedStreamsDelta,
        totalOutflowRate,
        totalAmountStreamedUntilUpdatedAt,
    } as ITokenStatistic;
};

export const getExpectedDataForFlowUpdated = async (
    testData: IFlowUpdatedUpdateTestData
) => {
    const {
        actionType,
        accountTokenSnapshots,
        flowRate,
        lastUpdatedAtTimestamp,
        lastUpdatedBlockNumber,
        superToken,
        pastStreamData,
        currentSenderATS,
        currentReceiverATS,
        currentTokenStats,
    } = testData;
    // newFlowRate - previousFlowRate
    const flowRateDelta = flowRate.sub(toBN(pastStreamData.oldFlowRate));

    // Update the data - we use this for comparison
    const updatedSenderATS = await updateAndReturnATSForCFAData(
        superToken,
        currentSenderATS,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        true,
        flowRate,
        flowRateDelta
    );
    const updatedReceiverATS = await updateAndReturnATSForCFAData(
        superToken,
        currentReceiverATS,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        false,
        flowRate,
        flowRateDelta
    );
    const updatedTokenStats = updateAndReturnTokenStatsForCFAData(
        currentTokenStats,
        accountTokenSnapshots,
        lastUpdatedBlockNumber,
        lastUpdatedAtTimestamp,
        actionType,
        flowRate,
        flowRateDelta
    );

    return { updatedSenderATS, updatedReceiverATS, updatedTokenStats };
};

export const getExpectedDataForRevokeOrDeleteSubscription = async (
    token: SuperToken,
    currentIndex: IIndex,
    currentSubscriber: ISubscriber,
    accountTokenSnapshots: { [id: string]: IAccountTokenSnapshot | undefined },
    currentPublisherATS: IAccountTokenSnapshot,
    currentSubscriberATS: IAccountTokenSnapshot,
    currentTokenStats: ITokenStatistic,
    isRevoke: boolean,
    userData: string,
    updatedAtBlock: string,
    timestamp: string
) => {
    const balanceDelta = toBN(currentIndex.newIndexValue)
        .sub(toBN(currentSubscriber.lastIndexValue))
        .mul(toBN(currentSubscriber.units));

    let updatedIndex: IIndex = {
        ...currentIndex,
    };
    let updatedSubscriber: ISubscriber = {
        ...currentSubscriber,
        userData,
        approved: false,
        totalAmountReceivedUntilUpdatedAt: toBN(
            currentSubscriber.totalAmountReceivedUntilUpdatedAt
        )
            .add(balanceDelta)
            .toString(),
        lastIndexValue: updatedIndex.newIndexValue,
    };
    let updatedPublisherATS: IAccountTokenSnapshot = {
        ...currentPublisherATS,
    };
    const accountTokenSnapshotsArray = Object.values(
        accountTokenSnapshots
    ).filter((x) => x != undefined) as IAccountTokenSnapshot[];
    let updatedTokenStats: ITokenStatistic = {
        ...updateAndReturnTokenStatsForCFAData(
            currentTokenStats,
            accountTokenSnapshotsArray,
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            toBN(0),
            toBN(0)
        ),
    };

    // this occurs whether subscription exists or not
    let updatedSubscriberATS: IAccountTokenSnapshot = {
        ...(await updateAndReturnATSForCFAData(
            token,
            currentSubscriberATS,
            updatedAtBlock,
            timestamp,
            FlowActionType.Update,
            true,
            toBN(0),
            toBN(0)
        )),
    };

    // handleRevokeOrDelete
    if (isRevoke) {
        updatedIndex = {
            ...updatedIndex,
            totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                .sub(currentSubscriber.units)
                .toString(),
            totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                .add(currentSubscriber.units)
                .toString(),
        };
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalApprovedSubscriptions:
                updatedSubscriberATS.totalApprovedSubscriptions - 1,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                updatedTokenStats.totalApprovedSubscriptions - 1,
        };
    } else {
        // isDelete
        updatedIndex = {
            ...updatedIndex,
            totalUnits: toBN(updatedIndex.totalUnits)
                .sub(currentSubscriber.units)
                .toString(),
            totalSubscribers: currentIndex.totalSubscribers - 1,
        };
        if (currentSubscriber.approved) {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsApproved: toBN(updatedIndex.totalUnitsApproved)
                    .sub(currentSubscriber.units)
                    .toString(),
            };
        } else {
            updatedIndex = {
                ...updatedIndex,
                totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                    .sub(currentSubscriber.units)
                    .toString(),
            };
        }
        updatedSubscriber = { ...updatedSubscriber, units: "0" };
        updatedSubscriberATS = {
            ...updatedSubscriberATS,
            totalSubscriptions: updatedSubscriberATS.totalSubscriptions - 1,
            totalApprovedSubscriptions:
                updatedSubscriberATS.totalApprovedSubscriptions - 1,
        };
        updatedTokenStats = {
            ...updatedTokenStats,
            totalApprovedSubscriptions:
                updatedTokenStats.totalApprovedSubscriptions - 1,
            totalSubscriptions: updatedTokenStats.totalSubscriptions - 1,
        };
    }

    if (currentSubscriber.approved === false) {
        updatedPublisherATS = {
            ...(await updateAndReturnATSForCFAData(
                token,
                updatedPublisherATS,
                updatedAtBlock,
                timestamp,
                FlowActionType.Update,
                true,
                toBN(0),
                toBN(0)
            )),
        };
    }

    return {
        updatedIndex,
        updatedSubscriber,
        updatedPublisherATS,
        updatedSubscriberATS,
        updatedTokenStats,
    };
};

/**************************************************************************
 * Modifier Functions
 *************************************************************************/

/**
 * Create/Update/Delete a flow between a sender and receiver.
 * Also waits for the graph to index and also returns the receipt
 * of the txn and data from the blockchain.
 * @param sf
 * @param cfaV1
 * @param actionType
 * @param superToken
 * @param sender
 * @param receiver
 * @param newFlowRate
 * @returns txnReceipt, flow updatedAt (on-chain), flowRate (current on-chain)
 */
export const modifyFlowAndReturnCreatedFlowData = async (
    provider: BaseProvider,
    sf: Framework,
    cfaV1: ConstantFlowAgreementV1,
    actionType: FlowActionType,
    superToken: string,
    sender: string,
    receiver: string,
    newFlowRate: number
) => {
    const actionToTypeStringMap = new Map([
        [FlowActionType.Create, "Create"],
        [FlowActionType.Update, "Update"],
        [FlowActionType.Delete, "Delete"],
    ]);
    console.log(
        `********************** ${actionToTypeStringMap.get(
            actionType
        )} a flow **********************`
    );
    const sfCFA = sf.cfa as ConstantFlowAgreementV1Helper;
    // any because it the txn.receipt doesn't exist on
    // Transaction
    const txn: any =
        actionType === FlowActionType.Create
            ? await sfCFA.createFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : actionType === FlowActionType.Update
            ? await sfCFA.updateFlow({
                  superToken,
                  sender,
                  receiver,
                  flowRate: newFlowRate.toString(),
                  userData: "0x",
                  onTransaction: () => {},
              })
            : await sfCFA.deleteFlow({
                  superToken,
                  sender,
                  receiver,
                  by: "",
                  userData: "0x",
                  onTransaction: () => {},
              });

    const receipt: ContractReceipt = txn.receipt;
    const block = await provider.getBlock(receipt.blockNumber);
    const timestamp = block.timestamp;
    await waitUntilBlockIndexed(receipt.blockNumber);

    const [, flowRate] = await cfaV1.getFlow(superToken, sender, receiver);

    return {
        receipt,
        timestamp,
        flowRate,
    };
};

export const hasSubscription = (
    subscribers: { [id: string]: ISubscriber | undefined },
    id: string
) => {
    return subscribers[id] != null;
};
