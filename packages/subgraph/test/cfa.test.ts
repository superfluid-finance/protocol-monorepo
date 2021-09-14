import { expect } from "chai";
import { ethers } from "hardhat";
import { request, gql } from "graphql-request";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    beforeSetup,
    monthlyToSecondRate,
    waitUntilBlockIndexed,
} from "./helpers/helpers";

describe("ConstantFlowAgreemntV1 Subgraph Tests", () => {
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
    });

    it("Should be able to create a flow.", async function (done: Mocha.Done) {
        const txn = await sf.cfa!.createFlow({
            superToken: daix.address,
            sender: userAddresses[0],
            receiver: userAddresses[3],
            flowRate: monthlyToSecondRate(100),
        });

        await waitUntilBlockIndexed(txn.receipt.blockNumber);
        const variables = {
            sender: userAddresses[0],
            receiver: userAddresses[3],
        };
        const query = gql`
            query getStreamEvent($sender: Bytes!, $receiver: Bytes!) {
                flowUpdateds(where: { sender: $sender, receiver: $receiver }) {
                    id
                    sender
                    receiver
                    flowRate
                    totalSenderFlowRate
                    totalReceiverFlowRate
                }
            }
        `;
        const data = await request(
            "http://localhost:8000/subgraphs/name/superfluid-test",
            query,
            variables
        );
        console.log(data);
        done();
    });

    it("Should be able to create multiple flows from one person to a few.", () => {});

    it("Should be able to create multiple flows from a few to one person.", () => {});
});
