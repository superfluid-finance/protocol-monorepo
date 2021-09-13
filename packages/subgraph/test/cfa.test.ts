import { expect } from "chai";
import { ethers } from "hardhat";
import { Framework } from "@superfluid-finance/js-sdk/src/Framework";
import { beforeSetup } from "./helpers/helpers";

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

    it("Should be able to create a flow.", () => {
    });


    it("Should be able to create multiple flows from one person to a few.", () => {
    });


    it("Should be able to create multiple flows from a few to one person.", () => {
    });
});
