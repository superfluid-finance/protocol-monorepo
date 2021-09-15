import { expect } from "chai";
import { ethers } from "hardhat";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import {
    beforeSetup,
	getEventId,
    monthlyToSecondRate,
	subgraphRequest,
    waitUntilBlockIndexed,
} from "./helpers/helpers";
import { getFlowUpdatedEventQuery, getFlowUpdatedEventsQuery } from "./queries/eventQueries";
import { IFlowUpdated, IQueryOptions } from "./interfaces";

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

    it("Should be able to create a flow.", async () => {
        const txn = await sf.cfa!.createFlow({
            superToken: daix.address,
            sender: userAddresses[0],
            receiver: userAddresses[3],
            flowRate: monthlyToSecondRate(100),
        });

        await waitUntilBlockIndexed(txn.receipt.blockNumber);

        const variables = {
            id: getEventId(txn.receipt)
        };

        const data = await subgraphRequest<IFlowUpdated>(
            getFlowUpdatedEventQuery,
            variables
        );
        console.log(data);
    });

    it("Should be able to create multiple flows from one person to a few.", () => {});

    it("Should be able to create multiple flows from a few to one person.", () => {});
});
