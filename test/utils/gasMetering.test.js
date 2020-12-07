const { expect } = require("chai");
const {
    BN
} = require("@openzeppelin/test-helpers");
const GasMetering = require("../../utils/gasMetering/gasMetering");

describe("GasMetering", function () {

    beforeEach(async () => {
        this.tx = {
            tx: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
            receipt: {
                transactionHash: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
                transactionIndex: 0,
                blockHash: "0x362333884391810d9a675c8639b4062abeb990604b25ff9ef3db6199d8901874",
                blockNumber: 35,
                from: "0xf17f52151ebef6c7334fad080c5704d77216b732",
                to: "0x9fbda871d559710256a2502a2517b794b482db40",
                gasUsed: 100000,
                cumulativeGasUsed: 100000,
                contractAddress: null,
                logs: [],
                status: true,
                logsBloom: "0x00...",
                rawLogs: [[{}]]
            },
            logs: []
        };

        this.gasPrice = "1000000000"; // 1gwei
    });

    it("should add transaction log", async () => {
        const gasMeter = new GasMetering("JSON", this.gasPrice, "USD", "400");

        gasMeter.pushTx(this.tx, "SomeAction");
        const record = gasMeter.records[0];
        expect(record.action).to.be.equal("SomeAction");
        expect(record.txHash).to.be.equal(this.tx.tx);
        expect(record.gas).to.be.bignumber.equal(new BN(100000));
        const expectedCost = new BN(100000).mul(new BN(this.gasPrice));
        expect(record.cost).to.be.bignumber.equal(expectedCost);

    });

    it("should calculate totals", async () => {
        const gasMeter = new GasMetering("JSON", this.gasPrice, "USD", "400");

        gasMeter.pushTx(this.tx, "SomeAction");
        gasMeter.pushTx(this.tx, "SomeAction2");
        const totals = gasMeter.totals;
        expect(totals.totalGas).to.be.bignumber.equal(new BN(200000));
        expect(totals.totalTx).to.be.bignumber.equal(new BN(2));
        const expectedCost = new BN(200000).mul(new BN(this.gasPrice));
        expect(totals.totalCost).to.be.bignumber.equal(expectedCost);
        const avgGas = new BN(200000).div(new BN(2));

        expect(totals.avgGas).to.be.bignumber.equal(avgGas);
        const avgCost = expectedCost.div(new BN(2));
        expect(totals.avgCost).to.be.bignumber.equal(avgCost);
        expect(totals.gasPrice).to.be.bignumber.equal(new BN(this.gasPrice));

    });

    it("should format correctly", async () => {
        const gasMeter = new GasMetering("JSON", this.gasPrice, "USD", "400");

        gasMeter.pushTx(this.tx, "SomeAction");
        gasMeter.pushTx(this.tx, "SomeAction2");
        const result = gasMeter._format();

        expect(result).to.be.deep.equal({
            totals: {
                totalTx: "2",
                totalGas: "200000",
                avgGas: "100000",
                gasPrice: "1 GWEI",
                totalCost: "0.0002 ETH",
                avgCost: "0.0001 ETH"
            },
            executedTxs: [{
                action: "SomeAction",
                txHash: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
                gas: "100000",
                cost: "0.0001 ETH"
            },
            {
                action: "SomeAction2",
                txHash: "0xf12344cf8a52ea36e2ba325c15b8faf6147d7fb98c900f39f50fec853f506286",
                gas: "100000",
                cost: "0.0001 ETH"
            }]
        });

    });


});