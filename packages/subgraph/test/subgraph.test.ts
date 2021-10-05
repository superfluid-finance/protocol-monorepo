import { ethers } from "hardhat";
import BN from "bn.js";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import { ERC20 } from "../typechain/ERC20";
import { SuperToken } from "../typechain/SuperToken";
import {
    beforeSetup,
    getOrInitializeDataForIDA,
    getRandomFlowRate,
    hasSubscription,
    toBN,
    updateAndReturnATSForCFAData,
    updateAndReturnIndexData,
    updateAndReturnTokenStatsForCFAData,
    waitUntilBlockIndexed,
} from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IBaseIDAEvent,
    IContracts,
    IExpectedSubscriberEvent,
    IExpectedSubscriptionUnitsUpdated,
    IIndex,
    IIndexCreated,
    IStreamData,
    IStreamLocalData,
    ISubscriber,
    ISubscriptionApproved,
    ISubscriptionRevoked,
    ISubscriptionUnitsUpdated,
    ITokenStatistic,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType } from "./helpers/constants";
import { validateModifyFlow, validateModifyIDA } from "./validation/validators";
import { InstantDistributionAgreementV1Helper } from "@superfluid-finance/js-sdk/src/InstantDistributionAgreementV1Helper";
import { ContractReceipt } from "@ethersproject/contracts";
import { fetchEventAndValidate } from "./validation/eventValidators";
import { fetchIndexAndValidate } from "./validation/holValidators";
import { fetchTokenStatsAndValidate } from "./validation/aggregateValidators";
import {
    getIndexCreatedEvents,
    getSubscriptionApprovedEvents,
    getSubscriptionRevokedEvents,
    getSubscriptionUnitsUpdatedEvents,
} from "./queries/eventQueries";

// TODO: Tests for totalSupply also needed
// TODO: Tests for reverse lookup fields needed
// probably can make a generalized function which can
// filter and fetch events of a particular contract

describe("Subgraph Tests", () => {
    const encoder = ethers.utils.defaultAbiCoder; // TODO: add some tests for userData
    let names: { [address: string]: string } = {};
    let userAddresses: string[] = [];
    let sf: Framework;
    let dai: ERC20;
    let daix: SuperToken;
    let cfaV1: ConstantFlowAgreementV1;
    let idaV1: InstantDistributionAgreementV1;
    let provider = ethers.getDefaultProvider("http://0.0.0.0:8545");

    // A set of locally updated variables to compare with data from the Graph.
    // The data in here comes from
    let revisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token
    let streamData: { [id: string]: IStreamData | undefined } = {}; // id is stream id
    let indexes: { [id: string]: IIndex | undefined } = {}; // id is index id
    let subscribers: { [id: string]: ISubscriber | undefined } = {}; // id is subscriber id
    let accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    } = {}; // id is ats id
    let tokenStatistics: { [id: string]: ITokenStatistic | undefined } = {}; // id is tokenStats id

    function updateGlobalObjectsForFlowUpdated(
        revisionIndexId: string,
        updatedStreamData: IStreamData,
        updatedSenderATS: IAccountTokenSnapshot,
        updatedReceiverATS: IAccountTokenSnapshot,
        updatedTokenStats: ITokenStatistic
    ) {
        revisionIndexes[revisionIndexId] = Number(
            updatedStreamData.revisionIndex
        );
        streamData[updatedStreamData.id] = updatedStreamData;
        accountTokenSnapshots[updatedSenderATS.id] = updatedSenderATS;
        accountTokenSnapshots[updatedReceiverATS.id] = updatedReceiverATS;
        tokenStatistics[updatedTokenStats.id] = updatedTokenStats;
    }

    function updateGlobalObjectsForIDAEvents(
        updatedTokenStats: ITokenStatistic,
        updatedIndex?: IIndex,
        updatedSubscriber?: ISubscriber,
        updatedPublisherATS?: IAccountTokenSnapshot,
        updatedSubscriberATS?: IAccountTokenSnapshot
    ) {
        tokenStatistics[updatedTokenStats.id] = updatedTokenStats;
        if (updatedIndex) {
            indexes[updatedIndex.id] = updatedIndex;
        }
        if (updatedSubscriber) {
            subscribers[updatedSubscriber.id] = updatedSubscriber;
        }
        if (updatedPublisherATS) {
            accountTokenSnapshots[updatedPublisherATS.id] = updatedPublisherATS;
        }
        if (updatedSubscriberATS) {
            accountTokenSnapshots[updatedSubscriberATS.id] =
                updatedSubscriberATS;
        }
    }

    function getContracts(): IContracts {
        return {
            cfaV1,
            sf,
            superToken: daix,
        };
    }

    function getStreamLocalData(): IStreamLocalData {
        return {
            accountTokenSnapshots,
            revisionIndexes,
            streamData,
            tokenStatistics,
        };
    }

    before(async () => {
        let [Names, UserAddresses, SF, DAI, DAIx] = await beforeSetup(100000);
        names = Names;
        userAddresses = UserAddresses;
        sf = SF;
        dai = DAI;
        daix = DAIx;
        cfaV1 = (await ethers.getContractAt(
            cfaABI,
            localAddresses.cfaAddress
        )) as ConstantFlowAgreementV1;
        idaV1 = (await ethers.getContractAt(
            idaABI,
            localAddresses.idaAddress
        )) as InstantDistributionAgreementV1;
    });

    describe("ConstantFlowAgreementV1 Tests", () => {
        /**
         * Flow Creation Tests
         */
        it("Should return correct data after creating multiple flows from one person to many.", async () => {
            // Deployer to Alice...Frank
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        it("Should return correct data after creating multiple flows from many to one person.", async () => {
            // Alice...Frank to Deployer
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        // /**
        //  * Flow Update Tests
        //  */
        it("Should return correct data after updating multiple flows from one person to many.", async () => {
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length; i++) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        it("Should return correct data after updating multiple flows from many to one person.", async () => {
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length / 2; i++) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }

            for (let i = 1; i < userAddresses.length / 2; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        /**
         * Flow Delete Tests
         */
        it("Should return correct data after deleting a created flow.", async () => {
            // delete the updated flows
            for (
                let i = userAddresses.length / 2;
                i < userAddresses.length;
                i++
            ) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Delete,
                    0,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        it("Should return correct data after deleting an updated flow.", async () => {
            // delete the updated flows
            for (let i = 1; i < userAddresses.length; i++) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Delete,
                    0,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        it("Should return correct data after creating a flow after deleting.", async () => {
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });

        it("Should return correct data after updating a flow after deleting.", async () => {
            const randomFlowRate = getRandomFlowRate(1000);
            for (let i = 1; i < userAddresses.length; i++) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
                    getContracts(),
                    getStreamLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats
                );
            }
        });
    });

    // GOTCHA'S: The balance may be inconsistent, so we'll have to use a web3 call + the subgraph data
    // when a user claims tokens
    describe("InstantDistributionAgreement Tests", () => {
        /**
         * Create Index Tests
         */
        it("Should return correct data after multiple users create multiple indexes", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                const publisher = userAddresses[i];
                const txn: any = await (
                    sf.ida as InstantDistributionAgreementV1Helper
                ).createIndex({
                    superToken: token,
                    publisher,
                    indexId: i,
                    userData: "0x",
                    onTransaction: () => {},
                });

                const receipt: ContractReceipt = txn.receipt;
                const block = await provider.getBlock(receipt.blockNumber);
                const timestamp = block.timestamp.toString();
                await waitUntilBlockIndexed(receipt.blockNumber);

                const updatedAtBlock = receipt.blockNumber.toString();

                await fetchEventAndValidate<IIndexCreated, IBaseIDAEvent>(
                    receipt,
                    {
                        token: token.toLowerCase(),
                        publisher: publisher.toLowerCase(),
                        indexId: i.toString(),
                    },
                    getIndexCreatedEvents,
                    "indexCreateds",
                    "IndexCreated"
                );

                let { currentIndex, currentTokenStats } =
                    getOrInitializeDataForIDA({
                        accountTokenSnapshots,
                        indexes,
                        indexId: i.toString(),
                        lastUpdatedAtTimestamp: timestamp,
                        lastUpdatedBlockNumber: updatedAtBlock,
                        publisher,
                        subscribers,
                        token,
                        tokenStatistics,
                    });

                const accountTokenSnapshotsArray = Object.values(
                    accountTokenSnapshots
                ).filter((x) => x != undefined) as IAccountTokenSnapshot[];
                let updatedTokenStats = updateAndReturnTokenStatsForCFAData(
                    currentTokenStats,
                    accountTokenSnapshotsArray,
                    updatedAtBlock,
                    timestamp.toString(),
                    FlowActionType.Update,
                    toBN(0),
                    toBN(0)
                );
                updatedTokenStats = {
                    ...updatedTokenStats,
                    totalNumberOfIndexes:
                        updatedTokenStats.totalNumberOfIndexes + 1,
                };

                await fetchIndexAndValidate(idaV1, currentIndex);
                await fetchTokenStatsAndValidate(
                    token.toLowerCase(),
                    updatedTokenStats
                );

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    currentIndex
                );
            }
        });

        /**
         * Approve Subscription Tests (as Subscriber)
         */
        it("Should return correct data after multiple non-subscribed users approve subscriptions to multiple indexes", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                // Take this code and put it into a function (validateSubscriptionApproved)
                const publisher = userAddresses[i];
                const subscriber = userAddresses[i - 1];
                const userData = "0x";
                const txn: any = await (
                    sf.ida as InstantDistributionAgreementV1Helper
                ).approveSubscription({
                    superToken: token,
                    publisher,
                    indexId: i,
                    subscriber,
                    userData,
                    onTransaction: () => {},
                });
                const receipt: ContractReceipt = txn.receipt;
                const block = await provider.getBlock(receipt.blockNumber);
                const timestamp = block.timestamp.toString();
                await waitUntilBlockIndexed(receipt.blockNumber);

                const updatedAtBlock = receipt.blockNumber.toString();

                let {
                    subscriberEntityId,
                    currentIndex,
                    currentSubscriber,
                    currentPublisherATS,
                    currentSubscriberATS,
                    currentTokenStats,
                } = getOrInitializeDataForIDA({
                    accountTokenSnapshots,
                    indexes,
                    indexId: i.toString(),
                    lastUpdatedAtTimestamp: timestamp,
                    lastUpdatedBlockNumber: updatedAtBlock,
                    publisher,
                    subscribers,
                    subscriber,
                    token,
                    tokenStatistics,
                });

                await fetchEventAndValidate<
                    ISubscriptionApproved,
                    IExpectedSubscriberEvent
                >(
                    receipt,
                    {
                        token: token.toLowerCase(),
                        publisher: publisher.toLowerCase(),
                        indexId: i.toString(),
                        subscriber: { id: subscriberEntityId },
                    },
                    getSubscriptionApprovedEvents,
                    "subscriptionApproveds",
                    "SubscriptionApproved"
                );

                const subscriptionExists = hasSubscription(
                    subscribers,
                    subscriberEntityId
                );

                const balanceDelta = toBN(currentIndex.newIndexValue)
                    .sub(toBN(currentSubscriber.lastIndexValue))
                    .mul(toBN(currentSubscriber.units));

                let updatedIndex = { ...currentIndex };
                let updatedSubscriber = {
                    ...currentSubscriber,
                    userData,
                    approved: true,
                    lastIndexValue: currentIndex.newIndexValue,
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
                    totalApprovedSubscriptions:
                        currentTokenStats.totalApprovedSubscriptions + 1,
                };

                // this occurs whether subscription exists or not
                let updatedSubscriberATS: IAccountTokenSnapshot = {
                    ...(await updateAndReturnATSForCFAData(
                        daix,
                        currentSubscriberATS,
                        updatedAtBlock,
                        timestamp,
                        FlowActionType.Update,
                        true,
                        toBN(0),
                        toBN(0)
                    )),
                    totalApprovedSubscriptions:
                        currentSubscriberATS.totalApprovedSubscriptions + 1,
                };

                if (subscriptionExists === true) {
                    // Update Index
                    updatedIndex = updateAndReturnIndexData(currentIndex, {
                        totalUnitsApproved: toBN(
                            currentIndex.totalUnitsApproved
                        ).add(toBN(currentSubscriber.units)),
                        totalUnitsPending: toBN(
                            currentIndex.totalUnitsPending
                        ).sub(toBN(currentSubscriber.units)),
                    });

                    // Update Subscriber
                    updatedSubscriber = {
                        ...updatedSubscriber,
                        totalAmountReceivedUntilUpdatedAt: toBN(
                            updatedSubscriber.totalAmountReceivedUntilUpdatedAt
                        )
                            .add(balanceDelta)
                            .toString(),
                    };

                    // Update Publisher ATS entity (stream data + balance)
                    updatedPublisherATS = await updateAndReturnATSForCFAData(
                        daix,
                        currentPublisherATS,
                        updatedAtBlock,
                        timestamp,
                        FlowActionType.Update,
                        true,
                        toBN(0),
                        toBN(0)
                    );
                } else {
                    // Update Subscriber entity
                    updatedIndex = {
                        ...updatedIndex,
                        totalSubscribers: updatedIndex.totalSubscribers + 1,
                    };
                    // Update Subscriber ATS entity
                    updatedSubscriberATS = {
                        ...updatedSubscriberATS,
                        totalSubscriptions:
                            currentSubscriberATS.totalSubscriptions + 1,
                    };

                    // Update Token Stats
                    updatedTokenStats = {
                        ...updatedTokenStats,
                        totalSubscriptions:
                            updatedTokenStats.totalSubscriptions + 1,
                    };
                }

                await validateModifyIDA(
                    idaV1,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                    updatedTokenStats,
                    token,
                    publisher,
                    subscriber
                );

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS
                );
            }
        });

        /**
         * Update Subscription Units Tests (as Publisher)
         */
        it("Should return correct data after a publisher updates non-subscribed users' subscription units", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                // Take this code and put it into a function (validateUpdateSubscriptionUnits)
                const publisher = userAddresses[i];
                const subscriber = userAddresses[i - 1];
                const userData = "0x";
                const units = new BN(100);
                const txn: any = await (
                    sf.ida as InstantDistributionAgreementV1Helper
                ).updateSubscription({
                    superToken: token,
                    publisher,
                    indexId: i,
                    subscriber,
                    units,
                    userData,
                    onTransaction: () => {},
                });
                const receipt: ContractReceipt = txn.receipt;
                const block = await provider.getBlock(receipt.blockNumber);
                const timestamp = block.timestamp.toString();
                await waitUntilBlockIndexed(receipt.blockNumber);

                const updatedAtBlock = receipt.blockNumber.toString();

                let {
                    subscriberEntityId,
                    currentIndex,
                    currentSubscriber,
                    currentPublisherATS,
                    currentSubscriberATS,
                    currentTokenStats,
                } = getOrInitializeDataForIDA({
                    accountTokenSnapshots,
                    indexes,
                    indexId: i.toString(),
                    lastUpdatedAtTimestamp: timestamp,
                    lastUpdatedBlockNumber: updatedAtBlock,
                    publisher,
                    subscribers,
                    subscriber,
                    token,
                    tokenStatistics,
                });

                await fetchEventAndValidate<
                    ISubscriptionUnitsUpdated,
                    IExpectedSubscriptionUnitsUpdated
                >(
                    receipt,
                    {
                        token: token.toLowerCase(),
                        publisher: publisher.toLowerCase(),
                        indexId: i.toString(),
                        subscriber: { id: subscriberEntityId },
                        units: units.toString(),
                    },
                    getSubscriptionUnitsUpdatedEvents,
                    "subscriptionUnitsUpdateds",
                    "SubscriptionUnitsUpdated"
                );

                const subscriptionExists = hasSubscription(
                    subscribers,
                    subscriberEntityId
                );

                // ends here

                // this is unique to updateSubUnits

                let updatedIndex = { ...currentIndex };
                let updatedSubscriber = { ...currentSubscriber };
                let updatedPublisherATS = { ...currentPublisherATS };
                let updatedSubscriberATS = {
                    ...currentSubscriberATS,
                    ...(await updateAndReturnATSForCFAData(
                        daix,
                        currentSubscriberATS,
                        updatedAtBlock,
                        timestamp,
                        FlowActionType.Update,
                        true,
                        toBN(0),
                        toBN(0)
                    )),
                };
                let updatedTokenStats = { ...currentTokenStats };

                const unitsDelta = toBN(units.toString()).sub(
                    toBN(currentSubscriber.units)
                );
                if (subscriptionExists && currentSubscriber.approved) {
                    updatedIndex = {
                        ...updatedIndex,
                        totalUnitsApproved: toBN(
                            updatedIndex.totalUnitsApproved
                        )
                            .add(unitsDelta)
                            .toString(),
                        totalUnits: toBN(updatedIndex.totalUnits)
                            .add(unitsDelta)
                            .toString(),
                    };
                } else if (subscriptionExists) {
                    updatedIndex = {
                        ...updatedIndex,
                        totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                            .add(unitsDelta)
                            .toString(),
                        totalUnits: toBN(updatedIndex.totalUnits)
                            .add(unitsDelta)
                            .toString(),
                    };
                } else {
                    updatedIndex = {
                        ...updatedIndex,
                        totalUnitsPending: toBN(updatedIndex.totalUnitsPending)
                            .add(toBN(units))
                            .toString(),
                        totalUnits: toBN(updatedIndex.totalUnits)
                            .add(toBN(units))
                            .toString(),
                        totalSubscribers: updatedIndex.totalSubscribers + 1,
                    };

                    updatedSubscriber = {
                        ...updatedSubscriber,
                        lastIndexValue: updatedIndex.newIndexValue,
                        units: units.toString(),
                    };

                    updatedSubscriberATS = {
                        ...updatedSubscriberATS,
                        totalSubscriptions:
                            updatedSubscriberATS.totalSubscriptions + 1,
                    };
                    const accountTokenSnapshotsArray = Object.values(
                        accountTokenSnapshots
                    ).filter((x) => x != undefined) as IAccountTokenSnapshot[];
                    updatedTokenStats = {
                        ...updateAndReturnTokenStatsForCFAData(
                            updatedTokenStats,
                            accountTokenSnapshotsArray,
                            updatedAtBlock,
                            timestamp,
                            FlowActionType.Update,
                            toBN(0),
                            toBN(0)
                        ),
                        totalSubscriptions:
                            updatedTokenStats.totalSubscriptions + 1,
                    };
                }

                const balanceDelta = toBN(updatedSubscriber.units).mul(
                    toBN(updatedIndex.newIndexValue).sub(
                        toBN(updatedSubscriber.lastIndexValue)
                    )
                );

                updatedSubscriber = {
                    ...updatedSubscriber,
                    totalAmountReceivedUntilUpdatedAt: toBN(
                        updatedSubscriber.totalAmountReceivedUntilUpdatedAt
                    )
                        .add(balanceDelta)
                        .toString(),
                };

                if (currentSubscriber.approved === false) {
                    updatedPublisherATS = await updateAndReturnATSForCFAData(
                        daix,
                        currentPublisherATS,
                        updatedAtBlock,
                        timestamp,
                        FlowActionType.Update,
                        true,
                        toBN(0),
                        toBN(0)
                    );
                }

                if (subscriptionExists) {
                    updatedSubscriber = {
                        ...updatedSubscriber,
                        lastIndexValue: updatedIndex.newIndexValue,
                        units: units.toString(),
                    };
                }

                // uniqueness ends here (but this logic section should be abstracted)

                await validateModifyIDA(
                    idaV1,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                    updatedTokenStats,
                    token,
                    publisher,
                    subscriber
                );

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS
                );
            }
        });

        it("Should return correct data after a publisher updates non-approved subscribed users' subscription", async () => {
            /**
             * check the event entity (SubscriptionUnitsUpdated)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        // SubscriptionApproved
        it("Should return correct data after multiple subscribed users approve subscriptions to multiple indexes", async () => {
            /**
             * check the event entity (SubscriptionApproved)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        it("Should return correct data after a publisher updates approved subscribed users' subscription", async () => {
            /**
             * check the event entity (SubscriptionUnitsUpdated)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        /**
         * Revoke Subscription Tests (as subscriber)
         */
        it("Should return correct data after revoking a subscription.", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                // Take this code and put it into a function (it will be reused)
                const publisher = userAddresses[i];
                const subscriber = userAddresses[i - 1];
                const userData = "0x";
                const txn: any = await (
                    sf.ida as InstantDistributionAgreementV1Helper
                ).revokeSubscription({
                    superToken: token,
                    publisher,
                    indexId: i,
                    subscriber,
                    userData,
                    onTransaction: () => {},
                });
                const receipt: ContractReceipt = txn.receipt;
                const block = await provider.getBlock(receipt.blockNumber);
                const timestamp = block.timestamp.toString();
                await waitUntilBlockIndexed(receipt.blockNumber);

                const updatedAtBlock = receipt.blockNumber.toString();

                let {
                    subscriberEntityId,
                    currentIndex,
                    currentSubscriber,
                    currentPublisherATS,
                    currentSubscriberATS,
                    currentTokenStats,
                } = getOrInitializeDataForIDA({
                    accountTokenSnapshots,
                    indexes,
                    indexId: i.toString(),
                    lastUpdatedAtTimestamp: timestamp,
                    lastUpdatedBlockNumber: updatedAtBlock,
                    publisher,
                    subscribers,
                    subscriber,
                    token,
                    tokenStatistics,
                });

                await fetchEventAndValidate<
                    ISubscriptionRevoked,
                    IExpectedSubscriberEvent
                >(
                    receipt,
                    {
                        token: token.toLowerCase(),
                        publisher: publisher.toLowerCase(),
                        indexId: i.toString(),
                        subscriber: { id: subscriberEntityId },
                    },
                    getSubscriptionRevokedEvents,
                    "subscriptionRevokeds",
                    "SubscriptionRevoked"
                );

                const balanceDelta = toBN(currentIndex.newIndexValue)
                    .sub(toBN(currentSubscriber.lastIndexValue))
                    .mul(toBN(currentSubscriber.units));

                const isRevoke = true;

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
                        daix,
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
                        totalUnitsApproved: toBN(
                            updatedIndex.totalUnitsApproved
                        )
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
                            totalUnitsApproved: toBN(
                                updatedIndex.totalUnitsApproved
                            )
                                .sub(currentSubscriber.units)
                                .toString(),
                        };
                    } else {
                        updatedIndex = {
                            ...updatedIndex,
                            totalUnitsPending: toBN(
                                updatedIndex.totalUnitsPending
                            )
                                .sub(currentSubscriber.units)
                                .toString(),
                        };
                    }
                    updatedSubscriber = { ...updatedSubscriber, units: "0" };
                    updatedSubscriberATS = {
                        ...updatedSubscriberATS,
                        totalSubscriptions:
                            updatedSubscriberATS.totalSubscriptions - 1,
                        totalApprovedSubscriptions:
                            updatedSubscriberATS.totalApprovedSubscriptions - 1,
                    };
                    updatedTokenStats = {
                        ...updatedTokenStats,
                        totalApprovedSubscriptions:
                            updatedTokenStats.totalApprovedSubscriptions - 1,
                        totalSubscriptions:
                            updatedTokenStats.totalSubscriptions - 1,
                    };
                }

                if (currentSubscriber.approved === false) {
                    updatedPublisherATS = {
                        ...(await updateAndReturnATSForCFAData(
                            daix,
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

                await validateModifyIDA(
                    idaV1,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                    updatedTokenStats,
                    token,
                    publisher,
                    subscriber
                );

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscriber,
                    updatedPublisherATS,
                    updatedSubscriberATS
                );
            }
        });

        /**
         * Delete Subscription Tests
         */
        it("Should return correct data after deleting a subscription (as publisher).", async () => {
            /**
             * check the event entity (SubscriptionRevoked)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             * subscriber.units should be 0
             */
        });

        it("Should return correct data after deleting a subscription (as subscriber).", async () => {
            /**
             * check the event entity (SubscriptionRevoked)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             * subscriber.units should be 0
             */
        });

        /**
         * Claim Units Test
         */
        it("Should return correct data after claiming units.", async () => {
            /**
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        /**
         * Distribute Tests
         */
        it("Should return correct data after calling distribute to 0 subscribers", async () => {});

        it("Should return correct data after calling distribute to 0 approved subscribers", async () => {});

        it("Should return correct data after calling distribute to some approved subscribers", async () => {});

        it("Should return correct data after calling distribute to all approved subscribers", async () => {});

        /**
         * Update Index Tests
         */

        it("Should return correct data after calling update index with 0 subscribers", async () => {});

        it("Should return correct data after calling update index with 0 approved subscribers", async () => {});

        it("Should return correct data after calling update index with some approved subscribers", async () => {});

        it("Should return correct data after calling update index with all approved subscribers", async () => {});
    });
});
