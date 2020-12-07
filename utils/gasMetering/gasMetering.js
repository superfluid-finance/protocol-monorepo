const {
    BN
} = require("@openzeppelin/test-helpers");
const { web3 } = require("@openzeppelin/test-helpers/src/setup");
const {
    GasMeterJSONReporter,
    GasMeterHTMLReporter
} = require("./gasReporter");

class Formatter {
    static formatBigNumber(key, value) {
        switch (key) {
        case "cost":
        case "totalCost":
        case "avgCost":
            value = `${web3.utils.fromWei(value)} ETH`;
            break;
        case "totalTx":
        case "gas":
        case "totalGas":
        case "avgGas":
            value = value.toString(10);
            break;
        case "gasPrice":
            value = `${web3.utils.fromWei(value, "gwei")} GWEI`;
            break;
        }
        return value;

    }

    static formatObject(x) {
        const result = {};
        Object.keys(x).forEach((key) => {
            var value = x[key];
            if (BN.isBN(value)) {
                value = Formatter.formatBigNumber(key, value);
            }
            result[key] = value;
        });
        return result;
    }
}



module.exports = class GasMeter {

    constructor(outputFormat, gasPrice) {
        switch (outputFormat) {
        case "JSON":
            this.reporter = new GasMeterJSONReporter({});
            break;
        case "HTML":
            this.reporter = new GasMeterHTMLReporter({});
            break;
        case "TENDERLY":
            // TODO
            break;
        default:
            throw new Error(`Unsuported report type ${outputFormat}`);
        }
        this.gasPrice = new BN(gasPrice);
        this.records = [];
        this.totals = {
            totalTx: new BN("0"),
            totalGas: new BN("0"),
            avgGas: new BN("0"),
            gasPrice: this.gasPrice,
            totalCost: new BN("0"),
            avgCost: new BN("0")
        };
    }

    _format() {
        const formattedTotals = {};
        Object.keys(this.totals).forEach(key => {
            formattedTotals[key] = Formatter.formatBigNumber(key, this.totals[key], this.fiatCurr);
        });
        const formattedRecords = this.records.map(x => { return Formatter.formatObject(x, this.fiatCurr); });
        return {
            totals: { ...formattedTotals },
            executedTxs: [...formattedRecords]
        };
    }

    pushTx(tx, actionName) {
        const gas = new BN(tx.receipt.gasUsed);
        const cost = gas.mul(this.gasPrice);
        this.records.push({
            action: actionName,
            txHash: tx.tx,
            gas: gas,
            cost: cost
        });
        this.totals = {
            totalTx: this.totals.totalTx.add(new BN("1")),
            totalGas: this.totals.totalGas.add(gas),
            gasPrice: this.gasPrice,
            totalCost: this.totals.totalCost.add(cost)
        };
        this.totals.avgCost = this.totals.totalCost.div(this.totals.totalTx);
        this.totals.avgGas = this.totals.totalGas.div(this.totals.totalTx);

    }
    

    generateReport(name) {
        this.reporter.fileName = name;
        const formattedReport = this._format();
        this.reporter.generateReport(formattedReport);
    }

};