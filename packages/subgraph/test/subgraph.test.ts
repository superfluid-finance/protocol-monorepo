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
    IUpdateIDAGlobalObjects,
    ISubscriberDistributionTesterParams,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType, IDAEventType } from "./helpers/constants";
import { testFlowUpdated, testModifyIDA } from "./helpers/testers";

// TODO: validate Account entities reverse look up
// create generalized function to do this
describe("Subgraph Tests", () => {
    let userAddresses: string[] = [];
    let sf: Framework;
    let dai: ERC20;
    let daix: SuperToken;
    let cfaV1: ConstantFlowAgreementV1;
    let idaV1: InstantDistributionAgreementV1;
    let provider = ethers.getDefaultProvider("http://0.0.0.0:8545");
    let initialTotalSupply: string;

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

    function updateGlobalObjectsForIDAEvents(data: IUpdateIDAGlobalObjects) {
        const {
            updatedTokenStats,
            updatedPublisherATS,
            updatedSubscriberATS,
            updatedIndex,
            updatedSubscription,
        } = data;
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

    function getBaseIDAData(baseParams: ISubscriberDistributionTesterParams) {
        return {
            contracts: getContracts(),
            localData: getDistributionLocalData(),
            baseParams,
        };
    }

    before(async () => {
        let [UserAddresses, SF, DAI, DAIx, totalSupply] = await beforeSetup(
            100000
        );
        initialTotalSupply = totalSupply;
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
            // Deployer to All
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
                    totalSupply: initialTotalSupply,
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
            // All to Deployer
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
            // delete the updated flows from all to deployer
            for (let i = userAddresses.length; i < userAddresses.length; i++) {
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
            // delete the updated flows from deployer to all
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
            // recreate the deleted flows from deployer to all
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
            // update the recreated flows from deployer to all
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
        it("Should return correct data after multiple users create an index", async () => {
            const token = daix.address;
            for (let i = 0; i < userAddresses.length; i++) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    provider,
                    token,
                    publisher: userAddresses[i],
                    indexId: i,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber: "",
                };
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.IndexCreated,
                    })
                );
            }
        });

        it("Should return correct data after sub units updated twice, sub deleted, sub approved, sub units set to 0 and sub deleted.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (let i = 1; i < Math.floor(userAddresses.length / 2); i++) {
                const subscriber = userAddresses[i];
                const baseParams: ISubscriberDistributionTesterParams = {
                    provider,
                    token,
                    publisher: userAddresses[0],
                    indexId: 0,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };

                let units = new BN(100);

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN(150);

                // update sub units again
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // distribute units to non-approved subscribers
                const amountOrIndexValue = new BN(10);

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData({ ...baseParams, subscriber: "" }),
                        eventType: IDAEventType.IndexUpdated,
                        amountOrIndexValue,
                        isDistribute: false,
                    })
                );

                // delete subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: subscriber,
                    })
                );

                // approve deleted sub
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );

                // delete subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: subscriber,
                    })
                );

                // update sub units to > 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(100),
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );
            }
        });

        it("Should return correct data after subscribers without units are granted units, approved, have their sub units set to 0 and then have their subscription deleted.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const baseParams: ISubscriberDistributionTesterParams = {
                    provider,
                    token,
                    publisher: userAddresses[1],
                    indexId: 1,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };

                let units = new BN(100);

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // approve sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                units = new BN(0);

                // update approved sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // update sub units from 0 to 0 (we should be able to do this technically)
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // revoke approved sub w/o units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: true,
                        sender: subscriber,
                    })
                );

                // update revoked sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(150),
                    })
                );

                // approve revoked sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // revoke approved sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: true,
                        sender: subscriber,
                    })
                );

                // update sub units on revoked sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(175),
                    })
                );

                // update revoked sub w/ units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );
                // update revoked sub w/ units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );

                // update revoked sub w/o  units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );
            }
        });

        it("Should return correct data after approve sub without units, update sub units, distribute units and delete sub", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const baseParams: ISubscriberDistributionTesterParams = {
                    provider,
                    token,
                    publisher: userAddresses[2],
                    indexId: 2,
                    atsArray: getAccountTokenSnapshotsArray(),
                    userData: "0x",
                    subscriber,
                };

                // approve subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(100),
                    })
                );

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(150),
                    })
                );

                // distribute units to approved subscribers
                const amountOrIndexValue = new BN(100);
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData({ ...baseParams, subscriber: "" }),
                        eventType: IDAEventType.IndexUpdated,
                        amountOrIndexValue,
                        isDistribute: true,
                    })
                );

                // delete subs
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: subscriber,
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );

                // update not approved sub w/o units to 0 again
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(0),
                    })
                );

                // approve subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update approved sub w/o units to > 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN(150),
                    })
                );
            }
        });

        it("Should return correct data after non-subscribers get units, claim them and publisher updates their units to 0.", async () => {
            const token = daix.address;
            const multiBaseParams = {
                provider,
                token,
                publisher: userAddresses[3],
                indexId: 3,
                atsArray: getAccountTokenSnapshotsArray(),
                userData: "0x",
            };

            // approve and update sub units for half
            for (let i = 0; i < Math.floor(userAddresses.length / 2); i++) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // approve subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units
                let units = new BN(100);
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN(150);

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );
            }

            // update sub units for the other half
            for (
                let i = Math.floor(userAddresses.length / 2);
                i < userAddresses.length;
                i++
            ) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // update sub units
                let units = new BN(100);
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN(150);

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );
            }

            // distribute units to pending + claimed users
            const amountOrIndexValue = new BN(100);
            updateGlobalObjectsForIDAEvents(
                await testModifyIDA({
                    ...getBaseIDAData({ ...multiBaseParams, subscriber: "" }),
                    eventType: IDAEventType.IndexUpdated,
                    amountOrIndexValue,
                    isDistribute: true,
                })
            );

            for (
                let i = Math.floor(userAddresses.length / 2);
                i < userAddresses.length;
                i++
            ) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    ...multiBaseParams,
                    subscriber: userAddresses[i],
                };

                // claim pending units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams),
                        eventType: IDAEventType.Claim,
                    })
                );
            }
        });
    });
});
