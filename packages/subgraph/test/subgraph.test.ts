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
    IUpdateCFAGlobalObjects,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType, IDAEventType } from "./helpers/constants";
import { testFlowUpdated, testModifyIDA } from "./helpers/testers";
import { BaseProvider } from "@ethersproject/providers";
import { fetchTokenAndValidate } from "./validation/hol/tokenValidator";

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
    let periodRevisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token

    let streamData: { [id: string]: IStreamData | undefined } = {}; // id is stream id
    let indexes: { [id: string]: IIndex | undefined } = {}; // id is index id
    let subscription: { [id: string]: IIndexSubscription | undefined } = {}; // id is subscription id
    let accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    } = {}; // id is ats id
    let tokenStatistics: { [id: string]: ITokenStatistic | undefined } = {}; // id is tokenStats id

    function updateGlobalObjectsForFlowUpdated(data: IUpdateCFAGlobalObjects) {
        revisionIndexes[data.revisionIndexId] = Number(
            data.updatedStreamData.revisionIndex
        );
        periodRevisionIndexes[data.revisionIndexId] = Number(
            data.updatedStreamData.periodRevisionIndex
        );

        streamData[data.updatedStreamData.id] = data.updatedStreamData;
        accountTokenSnapshots[data.updatedSenderATS.id] = data.updatedSenderATS;
        accountTokenSnapshots[data.updatedReceiverATS.id] =
            data.updatedReceiverATS;
        tokenStatistics[data.updatedTokenStats.id] = data.updatedTokenStats;
    }

    function updateGlobalObjectsForIDAEvents(data: IUpdateIDAGlobalObjects) {
        tokenStatistics[data.updatedTokenStats.id] = data.updatedTokenStats;
        if (data.updatedIndex) {
            indexes[data.updatedIndex.id] = data.updatedIndex;
        }
        if (data.updatedSubscription) {
            subscription[data.updatedSubscription.id] =
                data.updatedSubscription;
        }
        if (data.updatedPublisherATS) {
            accountTokenSnapshots[data.updatedPublisherATS.id] =
                data.updatedPublisherATS;
        }
        if (data.updatedSubscriberATS) {
            accountTokenSnapshots[data.updatedSubscriberATS.id] =
                data.updatedSubscriberATS;
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
            periodRevisionIndexes,
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

    function getBaseIDAData(
        baseParams: ISubscriberDistributionTesterParams,
        provider: BaseProvider
    ) {
        return {
            contracts: getContracts(),
            localData: getDistributionLocalData(),
            baseParams,
            provider,
            atsArray: getAccountTokenSnapshotsArray(),
        };
    }
    function getBaseCFAData(provider: BaseProvider, tokenAddress: string) {
        return {
            contracts: getContracts(),
            localData: getStreamLocalData(),
            atsArray: getAccountTokenSnapshotsArray(),
            provider,
            tokenAddress,
        };
    }

    before(async () => {
        // NOTE: make the token symbol more customizable in the future
        let [UserAddresses, SF, DAI, DAIx, totalSupply] = await beforeSetup(
            10000000
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

    describe("Token Tests", () => {
        it("Should return the correct data for the superToken", async () => {
            await fetchTokenAndValidate(
                daix.address.toLowerCase(),
                "Super fDAI Fake Token",
                "fDAIx",
                true,
                dai.address,
                18
            );
        });

        it("Should return the correct data for the regularToken", async () => {
            await fetchTokenAndValidate(
                dai.address.toLowerCase(),
                "fDAI Fake Token",
                "fDAI",
                false,
                "",
                18
            );
        });
    });

    describe("ConstantFlowAgreementV1 Tests", () => {
        /**
         * Flow Creation Tests
         */
        it("Should return correct data after creating multiple flows from one person to many.", async () => {
            // Deployer to All
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Create,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                        totalSupply: initialTotalSupply,
                    })
                );
            }
        });

        it("Should return correct data after creating multiple flows from many to one person.", async () => {
            // All to Deployer
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Create,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[i],
                        receiver: userAddresses[0],
                    })
                );
            }
        });

        // /**
        //  * Flow Update Tests
        //  */
        it("Should return correct data after updating multiple flows from one person to many.", async () => {
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Update,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                    })
                );
            }

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Update,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                    })
                );
            }
        });

        it("Should return correct data after updating multiple flows from many to one person.", async () => {
            let randomFlowRate = getRandomFlowRate(1000) + 1000; // increased flowRate
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Update,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[i],
                        receiver: userAddresses[0],
                    })
                );
            }

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate

                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Update,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[i],
                        receiver: userAddresses[0],
                    })
                );
            }
        });

        /**
         * Flow Delete Tests
         */
        it("Should return correct data after deleting a created flow.", async () => {
            // delete the updated flows from all to deployer
            for (let i = userAddresses.length; i < userAddresses.length; i++) {
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Delete,
                        newFlowRate: 0,
                        sender: userAddresses[i],
                        receiver: userAddresses[0],
                    })
                );
            }
        });

        it("Should return correct data after deleting an updated flow.", async () => {
            // delete the updated flows from deployer to all
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Delete,
                        newFlowRate: 0,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                    })
                );
            }
        });

        it("Should return correct data after creating a flow after deleting.", async () => {
            // recreate the deleted flows from deployer to all
            for (let i = 1; i < userAddresses.length; i++) {
                const randomFlowRate = getRandomFlowRate(1000);
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Create,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                    })
                );
            }
        });

        it("Should return correct data after updating a flow after deleting.", async () => {
            // update the recreated flows from deployer to all
            const randomFlowRate = getRandomFlowRate(1000);
            for (let i = 1; i < userAddresses.length; i++) {
                // update the global environment objects
                updateGlobalObjectsForFlowUpdated(
                    await testFlowUpdated({
                        ...getBaseCFAData(provider, daix.address),
                        actionType: FlowActionType.Update,
                        newFlowRate: randomFlowRate,
                        sender: userAddresses[0],
                        receiver: userAddresses[i],
                    })
                );
            }
        });
    });

    /**
     * Note: The goal of the IDA tests are to test out all of the mapping function logic
     * by not always following the happy path, but rather trying out some alternative paths
     * which are unlikely, but still possible.
     * Also, please use the Subscription event type for actions which contain both a subscription
     * and index event type.
     */
    describe("InstantDistributionAgreement Tests", () => {
        /**
         * Create Index Tests
         */
        it("Should return correct data after multiple users create an index", async () => {
            const token = daix.address;
            for (let i = 0; i < userAddresses.length; i++) {
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher: userAddresses[i],
                    indexId: i,
                    userData: "0x",
                    subscriber: "",
                };
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.IndexCreated,
                    })
                );
            }
        });

        /**
         * Case #0:
         * 0 units not approved => not approved with units => not approved with units (diff)
         * => distribute units to non-approved with units => delete subscriptions
         * => approve deleted subscriptions => update deleted subscription units to 0
         * => delete approved subscription => update units to great than 0
         * => update units to 0
         */
        it("Should return correct data after handling case #0.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (let i = 1; i < Math.floor(userAddresses.length / 2); i++) {
                const subscriber = userAddresses[i];
                const publisher = userAddresses[0];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher,
                    indexId: 0,
                    userData: "0x",
                    subscriber,
                };

                let units = new BN((100 ** 18).toString());

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN((150 ** 18).toString());

                // update sub units again
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // distribute units to non-approved subscribers
                const amountOrIndexValue = new BN((20).toString());

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(
                            { ...baseParams, subscriber: "" },
                            provider
                        ),
                        eventType: IDAEventType.IndexUpdated,
                        amountOrIndexValue,
                        isDistribute: false,
                    })
                );

                // delete subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: publisher,
                    })
                );

                // approve deleted sub
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );

                // delete subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: publisher,
                    })
                );

                // update sub units to > 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((100 ** 18).toString()),
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );
            }
        });

        /**
         * Case #1:
         * 0 units not approved => not approved with units => approved with units
         * => update approved subscription with units to 0
         * => update approved subscription without units to 0
         * => revoke approved subscription without units
         * => update revoked subscription units to greater than 0
         * => approve revoked subscription with units
         * => revoke approved subscription with units
         * => update subscription units on revoked subscription with units
         * => update revoke subscription units to 0
         * => update revoke subscription to 0 (again)
         */
        it("Should return correct data after handling case #1.", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher: userAddresses[1],
                    indexId: 1,
                    userData: "0x",
                    subscriber,
                };

                let units = new BN((100 ** 18).toString());

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // approve sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                units = new BN((0 ** 18).toString());

                // update approved sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // update sub units from 0 to 0 (we should be able to do this technically)
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                // revoke approved sub w/o units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: true,
                        sender: subscriber,
                    })
                );

                // update revoked sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((150 ** 18).toString()),
                    })
                );

                // approve revoked sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // revoke approved sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: true,
                        sender: subscriber,
                    })
                );

                // update sub units on revoked sub w/ units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((175 ** 18).toString()),
                    })
                );

                // update revoked sub w/ units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );
                // update revoked sub w/o units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );
            }
        });

        /**
         * Case #2:
         * 0 units not approved => approved w/o units => approved with units
         * => approved w/ units => distribute units to approved subscribers
         * => delete approved subscriptions who received distribution
         * => update deleted subscription's units to 0
         * => update not approved subscriptions w/o units to 0 from 0
         * => approve not approved subscription w/o units
         * => update approved subscriptions units to greater than 0
         */
        it("Should return correct data after approve sub without units, update sub units, distribute units and delete sub", async () => {
            const token = daix.address;
            // Testing half the users on the last created index.
            for (
                let i = userAddresses.length / 2 + 1;
                i < userAddresses.length;
                i++
            ) {
                const subscriber = userAddresses[i];
                const publisher = userAddresses[2];
                const baseParams: ISubscriberDistributionTesterParams = {
                    token,
                    publisher,
                    indexId: 2,
                    userData: "0x",
                    subscriber,
                };

                // approve subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((100 ** 18).toString()),
                    })
                );

                // update sub units
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((150 ** 18).toString()),
                    })
                );

                // distribute units to approved subscribers
                const amountOrIndexValue = new BN((100 ** 18).toString());
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(
                            { ...baseParams, subscriber: "" },
                            provider
                        ),
                        eventType: IDAEventType.IndexUpdated,
                        amountOrIndexValue,
                        isDistribute: true,
                    })
                );

                // delete subs
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionRevoked,
                        isRevoke: false,
                        sender: publisher,
                    })
                );

                // update sub units to 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );

                // update not approved sub w/o units to 0 again
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((0 ** 18).toString()),
                    })
                );

                // approve subscriptions
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update approved sub w/o units to > 0
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units: new BN((150 ** 18).toString()),
                    })
                );
            }
        });

        /**
         * Case #3:
         * update from 0 units not approved => approve with units for half and
         * update from 0 units not approved => not approved with units for the other half
         * distribute units to approved + not approved users
         * non approved users claim pending tokens
         */
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
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionApproved,
                    })
                );

                // update sub units
                let units = new BN((100 ** 18).toString());
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN((150 ** 18).toString());

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
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
                let units = new BN((100 ** 18).toString());
                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );

                units = new BN((150 ** 18).toString());

                updateGlobalObjectsForIDAEvents(
                    await testModifyIDA({
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionUnitsUpdated,
                        units,
                    })
                );
            }

            // distribute units to pending + claimed users
            const amountOrIndexValue = new BN((200 ** 18).toString());
            updateGlobalObjectsForIDAEvents(
                await testModifyIDA({
                    ...getBaseIDAData(
                        { ...multiBaseParams, subscriber: "" },
                        provider
                    ),
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
                        ...getBaseIDAData(baseParams, provider),
                        eventType: IDAEventType.SubscriptionDistributionClaimed,
                    })
                );
            }
        });
    });
});
