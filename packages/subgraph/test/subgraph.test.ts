import { ethers } from "hardhat";
import BN from "bn.js";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import { ERC20 } from "../typechain/ERC20";
import { SuperToken } from "../typechain/SuperToken";
import { beforeSetup, getRandomFlowRate } from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IContracts,
    IDistributionLocalData,
    IIndex,
    IStreamData,
    IStreamLocalData,
    IIndexSubscription,
    ITokenStatistic,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType, IDAEventType } from "./helpers/constants";
import { testFlowUpdated, testModifyIDA } from "./helpers/testers";

// TODO: Tests for totalSupply also needed
// TODO: validate Account entities reverse look up
// create generalized function to do this
// TODO: go through the paths
// probably can make a generalized function which can
// filter and fetch events of a particular contract
describe("Subgraph Tests", () => {
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
    let subscription: { [id: string]: IIndexSubscription | undefined } = {}; // id is subscription id
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
        updatedSubscription?: IIndexSubscription,
        updatedPublisherATS?: IAccountTokenSnapshot,
        updatedSubscriberATS?: IAccountTokenSnapshot
    ) {
        tokenStatistics[updatedTokenStats.id] = updatedTokenStats;
        if (updatedIndex) {
            indexes[updatedIndex.id] = updatedIndex;
        }
        if (updatedSubscription) {
            subscription[updatedSubscription.id] = updatedSubscription;
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
            idaV1,
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

    function getDistributionLocalData(): IDistributionLocalData {
        return {
            accountTokenSnapshots,
            tokenStatistics,
            indexes,
            subscriptions: subscription,
        };
    }

    function getAccountTokenSnapshotsArray() {
        return Object.values(accountTokenSnapshots).filter(
            (x) => x != undefined
        ) as IAccountTokenSnapshot[];
    }

    before(async () => {
        let [UserAddresses, SF, DAI, DAIx] = await beforeSetup(100000);
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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Create,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Create,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    receiver: userAddresses[0],
                    tokenAddress: daix.address,
                });
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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Update,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Update,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Update,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    receiver: userAddresses[0],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Update,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[i],
                    receiver: userAddresses[0],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Delete,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: 0,
                    sender: userAddresses[i],
                    receiver: userAddresses[0],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Delete,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: 0,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Create,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                } = await testFlowUpdated({
                    contracts: getContracts(),
                    localData: getStreamLocalData(),
                    provider,
                    actionType: FlowActionType.Update,
                    atsArray: getAccountTokenSnapshotsArray(),
                    newFlowRate: randomFlowRate,
                    sender: userAddresses[0],
                    receiver: userAddresses[i],
                    tokenAddress: daix.address,
                });

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
                const baseParams = {
                    provider,
                    token,
                    publisher: userAddresses[i],
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber: "",
                };
                const { updatedIndex, updatedTokenStats } = await testModifyIDA(
                    {
                        contracts: getContracts(),
                        localData: getDistributionLocalData(),
                        baseParams,
                        eventType: IDAEventType.IndexCreated,
                    }
                );
                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex
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
                const baseParams = {
                    provider,
                    token,
                    publisher,
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };
                const {
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                } = await testModifyIDA({
                    contracts: getContracts(),
                    localData: getDistributionLocalData(),
                    baseParams,
                    eventType: IDAEventType.SubscriptionApproved,
                });

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
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
                const units = new BN(100);

                const baseParams = {
                    provider,
                    token,
                    publisher,
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };
                const {
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                } = await testModifyIDA({
                    contracts: getContracts(),
                    localData: getDistributionLocalData(),
                    baseParams,
                    eventType: IDAEventType.SubscriptionUnitsUpdated,
                    units,
                });

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
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

                const baseParams = {
                    provider,
                    token,
                    publisher,
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };

                const {
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                } = await testModifyIDA({
                    contracts: getContracts(),
                    localData: getDistributionLocalData(),
                    baseParams,
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: true,
                    sender: subscriber,
                });

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
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

        /**
         * Claim Units Test
         */
        it("Should return correct data after claiming units.", async () => {
            /**
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             * this is the funky one where you need to make an additional web3 call to get the correct amount because the subgraph will return an incorrect result.
             */
        });

        /**
         * Distribute Tests
         */
        it("Should return correct data after calling distribute to 0 subscribers", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                // Take this code and put it into a function (it will be reused)
                const publisher = userAddresses[i];
                const amountOrIndexValue = new BN(100);

                const baseParams = {
                    provider,
                    token,
                    publisher,
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber: "",
                };

                const {
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                } = await testModifyIDA({
                    contracts: getContracts(),
                    localData: getDistributionLocalData(),
                    baseParams,
                    eventType: IDAEventType.IndexUpdated,
                    amountOrIndexValue,
                    isDistribute: true,
                });

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS
                );
            }
        });

        it("Should return correct data after deleting a subscription (as subscriber).", async () => {
            const token = daix.address;
            for (let i = 1; i < userAddresses.length; i++) {
                // Take this code and put it into a function (it will be reused)
                const publisher = userAddresses[i];
                const subscriber = userAddresses[i - 1];

                const baseParams = {
                    provider,
                    token,
                    publisher,
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };

                const {
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS,
                } = await testModifyIDA({
                    contracts: getContracts(),
                    localData: getDistributionLocalData(),
                    baseParams,
                    eventType: IDAEventType.SubscriptionRevoked,
                    isRevoke: false,
                    sender: subscriber,
                });

                updateGlobalObjectsForIDAEvents(
                    updatedTokenStats,
                    updatedIndex,
                    updatedSubscription,
                    updatedPublisherATS,
                    updatedSubscriberATS
                );
            }
        });

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
