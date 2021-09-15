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

describe("Subgraph Tests", () => {
    let names: { [address: string]: string } = {};
    let userAddresses: string[] = [];
    let sf: Framework;
    let dai: any;
    let daix: any;

    before(async () => {
        let [Names, UserAddresses, SF, DAI, DAIx] = await beforeSetup(100000);
        names = Names;
        userAddresses = UserAddresses;
        sf = SF;
        dai = DAI;
        daix = DAIx;
        // const cfa = await ethers.getContractAt(
        //     cfaABI,
        //     localAddresses.cfaAddress
        // );
    });

    describe("ConstantFlowAgreementV1 Tests", () => {
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

            console.log(flowUpdatedEvent);
        });

        it("Should return correct data after creating multiple flows from one person to a few.", () => {});

        it("Should return correct data after creating multiple flows from a few to one person.", () => {});

        it("Should return correct data after updating a single flow.", () => {});

        it("Should return correct data after updating multiple flows from one person to a few.", () => {});

        it("Should return correct data after updating multiple flows from a few to one person.", () => {});

        it("Should return correct data after deleting a created flow.", () => {});

        it("Should return correct data after deleting an updated flow.", () => {});

		it("Should return correct data after creating a flow after deleting.", () => {});

		it("Should return correct data after creating and updating a flow after deleting.", () => {});
    });
});
