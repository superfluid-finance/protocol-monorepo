const { BN } = require("@openzeppelin/test-helpers");
const { web3 } = require("@openzeppelin/test-helpers/src/setup");
const { GasMeterJSONReporter, GasMeterHTMLReporter } = require("./gasReporter");

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
            case "minGas":
            case "maxGas":
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
        Object.keys(x).forEach(key => {
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
        this.aggregates = {};
    }

    _format() {
        const formattedAggregates = {};
        Object.keys(this.aggregates).forEach(actionName => {
            const bucket = this.aggregates[actionName];
            formattedAggregates[actionName] = {};
            Object.keys(bucket).forEach(key => {
                formattedAggregates[actionName][
                    key
                ] = Formatter.formatBigNumber(key, bucket[key], this.fiatCurr);
            });
        });
        const formattedRecords = this.records.map(x => {
            return Formatter.formatObject(x, this.fiatCurr);
        });
        return {
            aggregates: { ...formattedAggregates },
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
            gasPrice: this.gasPrice,
            cost: cost
        });
        if (!(actionName in this.aggregates)) {
            this.aggregates[actionName] = {
                action: actionName,
                avgGas: new BN("0"),
                avgCost: new BN("0"),
                minGas: new BN("10000000000000000000000"),
                maxGas: new BN("0"),
                totalTx: new BN("0"),
                totalGas: new BN("0"),
                totalCost: new BN("0")
            };
        }
        const bucket = this.aggregates[actionName];
        bucket.totalTx = bucket.totalTx.add(new BN("1"));
        bucket.totalGas = bucket.totalGas.add(gas);
        bucket.totalCost = bucket.totalCost.add(cost);
        bucket.avgCost = bucket.totalCost.div(bucket.totalTx);
        bucket.avgGas = bucket.totalGas.div(bucket.totalTx);
        bucket.minGas = BN.min(bucket.minGas, gas);
        bucket.maxGas = BN.max(bucket.maxGas, gas);
    }

    generateReport(name) {
        this.reporter.fileName = name;
        const formattedReport = this._format();
        this.reporter.generateReport(formattedReport);
    }
};
