export = GasMeter;
declare class GasMeter {
    constructor(web3: any, outputFormat: any, gasPrice: any);
    web3: any;
    BN: any;
    reporter: GasMeterJSONReporter | GasMeterHTMLReporter | undefined;
    gasPrice: any;
    records: any[];
    aggregates: {};
    formatter: Formatter;
    _format(): {
        aggregates: {};
        executedTxs: {}[];
    };
    pushTx(tx: any, actionName: any): void;
    generateReport(name: any): void;
}
import { GasMeterJSONReporter } from "./gasReporter";
import { GasMeterHTMLReporter } from "./gasReporter";
declare class Formatter {
    constructor(web3: any);
    web3: any;
    BN: any;
    formatBigNumber(key: any, value: any): any;
    formatObject(x: any): {};
}
