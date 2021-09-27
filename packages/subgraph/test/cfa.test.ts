import { ethers } from "hardhat";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import { ERC20 } from "../typechain/ERC20";
import { SuperToken } from "../typechain/SuperToken";
import { beforeSetup, monthlyToSecondRate } from "./helpers/helpers";
import { IAccountTokenSnapshot, ITokenStatistic } from "./interfaces";
import localAddresses from "../config/ganache.json";
import { validateModifyFlow } from "./validation/validators";

/**
 * TODO: it likely makes sense to have several global objects which persists throughout the lifetime of the tests:
 * - a TokenStatistics object
 * - a AccountTokenSnapshot object
 * - something to keep track of flow interactions between two individuals (oldFlowRate, previousUpdatedAt)
 */

interface IStreamHistory {
    currentRevisionIndex: string;
    oldFlowRate: string;
    previousUpdatedAt: number;
}

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
    let flowUpdatedHistory: { [id: string]: IStreamHistory } = {}; // id is streamId

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
            const { updatedATS, tokenStatistic } = await validateModifyFlow(
                sf,
                daix.address,
                sender,
                receiver,
                cfaV1,
                {
                    actionType: 0,
                    flowRate: 100,
                    oldFlowRate: "0", // must be formattedFlowRate
                    revisionIndex: "0",
                },
                accountTokenSnapshots,
                tokenStatistics
            );

            // update ATS data
            const senderATSId =
                sender.toLowerCase() + "-" + daix.address.toLowerCase();
            const receiverATSId =
                receiver.toLowerCase() + "-" + daix.address.toLowerCase();
            accountTokenSnapshots[senderATSId] = updatedATS[senderATSId];
            accountTokenSnapshots[receiverATSId] = updatedATS[receiverATSId];

            // TODO: update token stats, update flowUpdatedHistory
        });

        it("Should return correct data after creating multiple flows from one person to a few.", async () => {});

        it("Should return correct data after creating multiple flows from a few to one person.", async () => {});

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
