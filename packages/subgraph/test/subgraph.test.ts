import { ethers } from "hardhat";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import { ERC20 } from "../typechain/ERC20";
import { SuperToken } from "../typechain/SuperToken";
import {
    beforeSetup,
    getIndexId,
    getOrInitIndex,
    getOrInitTokenStatistics,
    getRandomFlowRate,
    toBN,
    updateAndReturnTokenStatsForCFAData,
    waitUntilBlockIndexed,
} from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IContracts,
    IIndex,
    ILocalData,
    IStreamData,
    ISubscriber,
    ITokenStatistic,
    IUpdateGlobalObjectData,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType } from "./helpers/constants";
import { validateModifyFlow } from "./validation/validators";
import { InstantDistributionAgreementV1Helper } from "@superfluid-finance/js-sdk/src/InstantDistributionAgreementV1Helper";
import { ContractReceipt } from "@ethersproject/contracts";
import { fetchIndexCreatedEventAndValidate } from "./validation/eventValidators";
import { BigNumber } from "@ethersproject/bignumber";

describe("Subgraph Tests", () => {
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

    function updateGlobalObjects({
        revisionIndexId,
        updatedStreamData,
        updatedSenderATS,
        updatedReceiverATS,
        updatedTokenStats,
        updatedIndex,
        updatedSubscriber,
    }: IUpdateGlobalObjectData) {
        if (updatedSenderATS) {
            accountTokenSnapshots[updatedSenderATS.id] = updatedSenderATS;
        }
        if (updatedReceiverATS) {
            accountTokenSnapshots[updatedReceiverATS.id] = updatedReceiverATS;
        }
        if (updatedIndex) {
            indexes[updatedIndex.id] = updatedIndex;
        }
        if (updatedSubscriber) {
            subscribers[updatedSubscriber.id] = updatedSubscriber;
        }
        if (revisionIndexId && updatedStreamData) {
            revisionIndexes[revisionIndexId] = Number(
                updatedStreamData.revisionIndex
            );
            streamData[updatedStreamData.id] = updatedStreamData;
        }
        if (updatedTokenStats) {
            tokenStatistics[updatedTokenStats.id] = updatedTokenStats;
        }
    }

    function getContracts(): IContracts {
        return {
            cfaV1,
            sf,
            superToken: daix,
        };
    }

    function getLocalData(): ILocalData {
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

    describe.only("ConstantFlowAgreementV1 Tests", () => {
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );
                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Delete,
                    0,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Delete,
                    0,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects({
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                });
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
                const timestamp = block.timestamp;
                await waitUntilBlockIndexed(receipt.blockNumber);

                const updatedAtBlock = receipt.blockNumber.toString();

                await fetchIndexCreatedEventAndValidate(
                    idaV1,
                    receipt,
                    token,
                    publisher,
                    i.toString()
                );

                const indexEntityId = getIndexId(
                    publisher,
                    token,
                    i.toString()
                );

                const currentIndex = getOrInitIndex(
                    indexes,
                    indexEntityId,
                    receipt.blockNumber.toString(),
                    timestamp.toString()
                );

                const currentTokenStats = getOrInitTokenStatistics(
                    tokenStatistics,
                    token.toLowerCase(),
                    updatedAtBlock,
                    timestamp.toString()
                );

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
                        updatedTokenStats.totalApprovedSubscriptions + 1,
                };
            }
            /**
             * check the event entity (IndexCreated)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * No need to update the data on StreamData though
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        /**
         * Approve Subscription Tests (as Subscriber)
         */
        it("Should return correct data after multiple non-subscribed users approve subscriptions to multiple indexes", async () => {
            /**
             * check the event entity (SubscriptionApproved)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        it("Should return correct data after multiple subscribed users approve subscriptions to multiple indexes", async () => {
            /**
             * check the event entity (SubscriptionApproved)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
        });

        /**
         * Update Subscription Tests (as Publisher)
         */
        it("Should return correct data after a publisher updates non-subscribed users' subscription", async () => {
            /**
             * check the event entity (SubscriptionUnitsUpdated)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
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
            /**
             * check the event entity (SubscriptionRevoked)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
             */
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
             */
        });

        it("Should return correct data after deleting a subscription (as subscriber).", async () => {
            /**
             * check the event entity (SubscriptionRevoked)
             * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index, Subscription)
             * update the aggregate data similar to the streams and compare (ATS, TokenStats)
             * remember to take into consideration the flowRate data here too
             * use toBN
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

        it("Should return correct data after calling distribute to all approved subscribers", async () => {});

        it("Should return correct data after calling distribute to some approved subscribers", async () => {});

        /**
         * Update Index Tests
         */

        it("Should return correct data after calling update index with 0 subscribers", async () => {});

        it("Should return correct data after calling update index with 0 approved subscribers", async () => {});

        it("Should return correct data after calling update index with all approved subscribers", async () => {});

        it("Should return correct data after calling update index with some approved subscribers", async () => {});
    });
});
