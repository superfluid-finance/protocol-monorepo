import { ethers } from "hardhat";
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
    ILocalData,
    IStreamData,
    ITokenStatistic,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { FlowActionType } from "./helpers/constants";
import { validateModifyFlow } from "./validation/validators";

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
    let streamData: { [id: string]: IStreamData | undefined } = {}; // id is streamId
    let accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    } = {}; // id is atsId
    let tokenStatistics: { [id: string]: ITokenStatistic | undefined } = {}; // id is tokenStatsId

    function updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );
                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[i],
                    userAddresses[0],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
            for (let i = userAddresses.length / 2; i < userAddresses.length; i++) {
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
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Delete,
                    0,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Create,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
                    getLocalData(),
                    provider,
                    FlowActionType.Update,
                    randomFlowRate,
                    userAddresses[0],
                    userAddresses[i],
                    daix.address
                );

                // update the global environment objects
                updateGlobalObjects(
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
	describe.skip("InstantDistributionAgreement Tests", () => {
		/**
         * Create Index Tests
         */
		it("Should return correct data after multiple users create multiple indexes", async () => {
			/**
			 * check the event entity (IndexCreated)
			 * check the HOL index entity with the returned data from sdk's idaHelper web3 (Index)
			 * update the aggregate data similar to the streams and compare (ATS, TokenStats)
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
		it("Should return correct data after calling distribute to 0 subscribers", async () => {

		});

		it("Should return correct data after calling distribute to 0 approved subscribers", async () => {

		});

		it("Should return correct data after calling distribute to all approved subscribers", async () => {

		});

		it("Should return correct data after calling distribute to some approved subscribers", async () => {

		});

		/**
         * Update Index Tests
         */

		 it("Should return correct data after calling update index with 0 subscribers", async () => {

		});

		it("Should return correct data after calling update index with 0 approved subscribers", async () => {

		});

		it("Should return correct data after calling update index with all approved subscribers", async () => {

		});

		it("Should return correct data after calling update index with some approved subscribers", async () => {

		});
	});
});
