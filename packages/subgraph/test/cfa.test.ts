import { expect } from "chai";
import { ethers } from "hardhat";
import { ContractReceipt } from "ethers";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    beforeSetup,
    getEventId,
    monthlyToSecondRate,
    subgraphRequest,
    waitUntilBlockIndexed,
} from "./helpers/helpers";
import { getFlowUpdatedEventQuery } from "./queries/eventQueries";
import { IFlowUpdated, ITokenStatistic } from "./interfaces";
import { validateEventData } from "./validation/validators";
import localAddresses from "../config/ganache.json";
import cfaABI from "../abis/IConstantFlowAgreementV1.json";
import idaABI from "../abis/IInstantDistributionAgreementV1.json";
import { ConstantFlowAgreementV1 } from "../typechain/ConstantFlowAgreementV1";
import { InstantDistributionAgreementV1 } from "../typechain/InstantDistributionAgreementV1";
import { ERC20 } from "../typechain/ERC20";
import { SuperToken } from "../typechain/SuperToken";

describe("Subgraph Tests", () => {
    let names: { [address: string]: string } = {};
    let userAddresses: string[] = [];
    let sf: Framework;
    let dai: ERC20;
    let daix: SuperToken;
    let cfaV1: ConstantFlowAgreementV1;
    let idaV1: InstantDistributionAgreementV1;

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
            const token = daix.address;
            const sender = userAddresses[0];
            const receiver = userAddresses[1];
            const flowRate = monthlyToSecondRate(100);
            const txn = await sf.cfa!.createFlow({
                superToken: token,
                sender,
                receiver,
                flowRate,
            });

            const receipt: ContractReceipt = txn.receipt;

            await waitUntilBlockIndexed(receipt.blockNumber);

            const variables = {
                id: getEventId(receipt),
            };

            const flowUpdatedEvent = await subgraphRequest<IFlowUpdated>(
                getFlowUpdatedEventQuery,
                variables
            );

            if (!flowUpdatedEvent) return;

            validateEventData(
                flowUpdatedEvent,
                {
                    token: token.toLowerCase(),
                    sender: sender.toLowerCase(),
                    receiver: receiver.toLowerCase(),
                    flowRate: flowRate.toString(),
                    totalSenderFlowRate: (-flowRate).toString(),
                    totalReceiverFlowRate: flowRate.toString(),
                    oldFlowRate: "0",
                    type: 0,
                },
                receipt
            );

            const [, contractFlowRate] = await cfaV1.getFlow(
                token,
                sender,
                receiver
            );
            const stringContractFlowRate = contractFlowRate.toString();
            expect(stringContractFlowRate).to.equal(flowUpdatedEvent.flowRate);
            expect(stringContractFlowRate);
        });

        it("Should return correct data after creating multiple flows from one person to a few.", () => {});

        it("Should return correct data after creating multiple flows from a few to one person.", () => {});

        /**
         * Flow Update Tests
         */
        it("Should return correct data after updating a single flow.", () => {});

        it("Should return correct data after updating multiple flows from one person to a few.", () => {});

        it("Should return correct data after updating multiple flows from a few to one person.", () => {});

        /**
         * Flow Delete Tests
         */
        it("Should return correct data after deleting a created flow.", () => {});

        it("Should return correct data after deleting an updated flow.", () => {});

        it("Should return correct data after creating a flow after deleting.", () => {});

        it("Should return correct data after creating and updating a flow after deleting.", () => {});
    });
});
