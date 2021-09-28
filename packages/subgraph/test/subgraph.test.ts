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
    getStreamId,
    monthlyToSecondRate,
} from "./helpers/helpers";
import {
    IAccountTokenSnapshot,
    IStreamHistory,
    ITokenStatistic,
} from "./interfaces";
import localAddresses from "../config/ganache.json";
import { validateModifyFlow } from "./validation/validators";
import {
    FlowActionType,
    INITIAL_ATS,
    INITIAL_STREAM_HISTORY,
} from "./helpers/constants";

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

    /**
     * TODO: create helper functions for updating these global properties
     * based on what is being modified (e.g. flow or ida)
     */
    let tokenStatistics: { [id: string]: ITokenStatistic } = {}; // id is tokenStatsId
    let accountTokenSnapshots: { [id: string]: IAccountTokenSnapshot } = {}; // id is atsId
    let streamHistory: { [id: string]: IStreamHistory } = {}; // id is streamId
    let revisionIndexes: { [id: string]: number | undefined } = {}; // id is sender-recipient-token

    async function validateModifyFlowAndUpdateGlobalProperties(
        sender: string,
        receiver: string,
        actionType: FlowActionType,
        flowRate: number
    ) {
        // streamHistory should be obtained within this function
        // if revisionIndex of sender-receipient, token is null
        // we use INITIAL_STREAM_HISTORY
        const currentRevisionIndex =
            revisionIndexes[
                sender.toLowerCase() +
                    "-" +
                    receiver.toLowerCase() +
                    "-" +
                    daix.address.toLowerCase()
            ];
        const currentStreamHistory = currentRevisionIndex
            ? streamHistory[
                  getStreamId(
                      sender,
                      receiver,
                      daix.address,
                      currentRevisionIndex.toString()
                  )
              ]
            : INITIAL_STREAM_HISTORY;
        console.log("accountTokenSnapshots", accountTokenSnapshots);
        const { updatedATS, updatedTokenStats, updatedStreamHistory } =
            await validateModifyFlow(
                sf,
                daix.address,
                sender,
                receiver,
                cfaV1,
                {
                    actionType,
                    flowRate,
                    streamHistory: currentStreamHistory,
                },
                accountTokenSnapshots,
                tokenStatistics
            );

        updateAndPrintGlobalProperties(
            sender,
            receiver,
            updatedTokenStats,
            updatedATS,
            updatedStreamHistory
        );
    }

    async function updateAndPrintGlobalProperties(
        sender: string,
        receiver: string,
        updatedTokenStats: ITokenStatistic,
        updatedATS: { [id: string]: IAccountTokenSnapshot },
        updatedStreamHistory?: IStreamHistory
    ) {
        const hexSender = sender.toLowerCase();
        const hexReceiver = receiver.toLowerCase();
        const hexToken = daix.address.toLowerCase();
        const senderATSId =
            sender.toLowerCase() + "-" + daix.address.toLowerCase();
        const receiverATSId =
            receiver.toLowerCase() + "-" + daix.address.toLowerCase();
        const oldTokenStats = tokenStatistics[hexToken];
        const oldSenderATS = accountTokenSnapshots[senderATSId];
        const oldReceiverATS = accountTokenSnapshots[receiverATSId];

        tokenStatistics[hexToken] = {
            ...tokenStatistics[hexToken],
            ...updatedTokenStats,
        };
        accountTokenSnapshots[senderATSId] = {
            ...accountTokenSnapshots[senderATSId],
            ...updatedATS[senderATSId],
        };
        accountTokenSnapshots[receiverATSId] = {
            ...accountTokenSnapshots[receiverATSId],
            ...updatedATS[receiverATSId],
        };

        console.log("Previous Token Stats: ", oldTokenStats);
        console.log("Updated Token Stats: ", tokenStatistics[hexToken], "\n");
        console.log("Previous Sender ATS: ", oldSenderATS);
        console.log(
            "Updated Sender ATS: ",
            accountTokenSnapshots[senderATSId],
            "\n"
        );
        console.log("Previous Receiver ATS: ", oldReceiverATS);
        console.log(
            "Updated Receiver ATS: ",
            accountTokenSnapshots[receiverATSId],
            "\n"
        );

        if (updatedStreamHistory) {
            const revisionIndexId =
                hexSender + "-" + hexReceiver + "-" + hexToken;
            const revisionIndex = revisionIndexes[revisionIndexId];
            const oldStreamId = getStreamId(
                hexSender,
                hexReceiver,
                hexToken,
                revisionIndex ? revisionIndex.toString() : "0"
            );
            console.log(
                "Previous Stream History: ",
                streamHistory[oldStreamId]
            );
            const streamId = getStreamId(
                hexSender,
                hexReceiver,
                hexToken,
                updatedStreamHistory.revisionIndex
            );
            revisionIndexes[revisionIndexId] = Number(
                updatedStreamHistory.revisionIndex
            );
            streamHistory[streamId] = {
                ...streamHistory[streamId],
                ...updatedStreamHistory,
            };
            console.log(
                "Updated Stream History: ",
                streamHistory[streamId],
                "\n"
            );
        }
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
        it("Should return correct data after creating a flow.", async () => {
            const sender = userAddresses[0];
            const receiver = userAddresses[1];
            await validateModifyFlowAndUpdateGlobalProperties(
                sender,
                receiver,
                FlowActionType.Create,
                monthlyToSecondRate(100)
            );
        });

        it("Should return correct data after creating multiple flows from one person to many.", async () => {
            const sender = userAddresses[1];
            for (let i = 2; i < userAddresses.length; i++) {
                let receiver = userAddresses[i];
                await validateModifyFlowAndUpdateGlobalProperties(
                    sender,
                    receiver,
                    FlowActionType.Create,
                    monthlyToSecondRate(250)
                );
            }
        });

        it("Should return correct data after creating multiple flows from many to one person.", async () => {});

        /**
         * Flow Update Tests
         */
        it("Should return correct data after updating a single flow.", async () => {});

        it("Should return correct data after updating multiple flows from one person to a few.", async () => {});

        it("Should return correct data after updating multiple flows from a few to one person.", async () => {});

        /**
         * Flow Delete Tests
         */
        it("Should return correct data after deleting a created flow.", async () => {});

        it("Should return correct data after deleting an updated flow.", async () => {});

        it("Should return correct data after creating a flow after deleting.", async () => {});

        it("Should return correct data after creating and updating a flow after deleting.", async () => {});
    });
});
