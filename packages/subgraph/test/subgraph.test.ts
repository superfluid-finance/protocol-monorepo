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
    getOrInitAccountTokenSnapshot,
    getOrInitRevisionIndex,
    getOrInitStreamData,
    getOrInitTokenStatic,
    getRandomFlowRate,
    getRevisionIndexId,
    getStreamId,
    monthlyToSecondRate,
    toBN,
    updateAndReturnATSOnFlowUpdated,
    updateAndReturnStreamData,
    updateAndReturnTokenStatsOnFlowUpdated,
} from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IStreamData,
    ITokenStatistic,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { modifyFlowAndReturnCreatedFlowData } from "./validation/validators";
import { FlowActionType } from "./helpers/constants";
import { fetchFlowUpdatedEventAndValidate } from "./validation/eventValidators";
import { fetchStreamAndValidate } from "./validation/holValidators";
import {
    fetchATSAndValidate,
    fetchTokenStatsAndValidate,
} from "./validation/aggregateValidators";

/**
 * TODO: it likely makes sense to have several global objects which persists throughout the lifetime of the tests:
 * - a TokenStatistics object
 * - a AccountTokenSnapshot object
 * - something to keep track of flow interactions between two individuals (oldFlowRate, previousUpdatedAt)
 */

/**
 * TODO: change up mapping to break the tests
 */

describe("Subgraph Tests", () => {
    let names: { [address: string]: string } = {};
    let userAddresses: string[] = [];
    let sf: Framework;
    let dai: ERC20;
    let daix: SuperToken;
    let cfaV1: ConstantFlowAgreementV1;
    let idaV1: InstantDistributionAgreementV1;

    // A set of locally updated variables to compare with data from the Graph.
    // The data in here comes from
    let revisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token
    let streamData: { [id: string]: IStreamData | undefined } = {}; // id is streamId
    let accountTokenSnapshots: {
        [id: string]: IAccountTokenSnapshot | undefined;
    } = {}; // id is atsId
    let tokenStatistics: { [id: string]: ITokenStatistic | undefined } = {}; // id is tokenStatsId

    async function validateModifyFlow(
        actionType: FlowActionType,
        newFlowRate: number,
        sender: string,
        receiver: string,
        token: string
    ) {
        // CREATE A FLOW
        const { receipt, updatedAtTimestamp, flowRate } =
            await modifyFlowAndReturnCreatedFlowData(
                sf,
                cfaV1,
                actionType,
                daix.address,
                sender,
                receiver,
                monthlyToSecondRate(newFlowRate)
            );
        const lastUpdatedAtTimestamp = updatedAtTimestamp.toString();
        const lastUpdatedBlockNumber = receipt.blockNumber.toString();
        const tokenId = token.toLowerCase();

        // GET OR INIT THE DATA
        const revisionIndexId = getRevisionIndexId(sender, receiver, token);
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
        const currentTokenStats = getOrInitTokenStatic(
            tokenStatistics,
            tokenId,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp
        );

        // newFlowRate - previousFlowRate
        const flowRateDelta = flowRate.sub(toBN(pastStreamData.oldFlowRate));

        // Update the data - we use this for comparison
        const updatedSenderATS = await updateAndReturnATSOnFlowUpdated(
            daix,
            currentSenderATS,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            actionType,
            true,
            flowRate,
            flowRateDelta
        );
        const updatedReceiverATS = await updateAndReturnATSOnFlowUpdated(
            daix,
            currentReceiverATS,
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            actionType,
            false,
            flowRate,
            flowRateDelta
        );
        const updatedTokenStats = updateAndReturnTokenStatsOnFlowUpdated(
            currentTokenStats,
            Object.values(accountTokenSnapshots).filter(
                (x) => x != undefined
            ) as IAccountTokenSnapshot[],
            lastUpdatedBlockNumber,
            lastUpdatedAtTimestamp,
            actionType,
            flowRate,
            flowRateDelta
        );
        const streamedAmountSinceUpdatedAt = toBN(lastUpdatedAtTimestamp)
            .sub(toBN(pastStreamData.lastUpdatedAtTimestamp))
            .mul(toBN(pastStreamData.oldFlowRate));

        // validate FlowUpdatedEvent
        await fetchFlowUpdatedEventAndValidate(
            cfaV1,
            receipt,
            token,
            sender,
            receiver,
            flowRate.toString(),
            pastStreamData.oldFlowRate,
            actionType
        );

        // validate Stream HOL
        await fetchStreamAndValidate(
            pastStreamData,
            streamedAmountSinceUpdatedAt,
            flowRate.toString()
        );

        // validate sender ATS
        await fetchATSAndValidate(currentSenderATS.id, updatedSenderATS);

        // validate receiver ATS
        await fetchATSAndValidate(currentReceiverATS.id, updatedReceiverATS);

        // validate token stats
        await fetchTokenStatsAndValidate(tokenId, updatedTokenStats);

        let updatedStreamData = updateAndReturnStreamData(
            pastStreamData,
            actionType,
            flowRate.toString(),
            lastUpdatedAtTimestamp,
            streamedAmountSinceUpdatedAt
        );

        return {
            revisionIndexId,
            updatedStreamData,
            updatedReceiverATS,
            updatedSenderATS,
            updatedTokenStats,
        };
    }

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
        console.log("updatedTokenStats", updatedTokenStats);
        tokenStatistics[updatedTokenStats.id] = updatedTokenStats;
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
            for (let i = 1; i < userAddresses.length; i++) {
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
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

            for (let i = 1; i < userAddresses.length; i++) {
                randomFlowRate = getRandomFlowRate(1000); // decreased flowRate
                const {
                    revisionIndexId,
                    updatedStreamData,
                    updatedSenderATS,
                    updatedReceiverATS,
                    updatedTokenStats,
                } = await validateModifyFlow(
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
		});

        it("Should return correct data after deleting an updated flow.", async () => {
		});

        it("Should return correct data after creating a flow after deleting.", async () => {});

        it("Should return correct data after creating and updating a flow after deleting.", async () => {});
    });
});
