import type BN from 'bn.js'
import type Web3 from 'web3'
import type { GasMeterJSONReporter } from "./gasReporter";
import type { GasMeterHTMLReporter } from "./gasReporter";

export type Record = {
    action: string
    txHash: string
    gas: BN
    gasPrice: BN
    cost: BN
}

export declare class GasMeter {
    constructor(
        web3: Web3,
        outputFormat: 'JSON'|'HTML'|'TENDERLY',
        gasPrice: string
    );
    web3: Web3;
    BN: BN;
    reporter: GasMeterJSONReporter | GasMeterHTMLReporter | undefined;
    gasPrice: string;
    records: Record[];
    aggregates: {};
    formatter: Formatter;
    _format(): {
        aggregates: {};
        executedTxs: {}[];
    };
    pushTx(
        tx: Record,
        actionName: string
    ): void;
    generateReport(name: string): void;
}
declare class Formatter {
    constructor(web3: Web3);
    web3: Web3;
    BN: BN;
    formatBigNumber(key: string, value: number | BN): string;
    formatObject(x: Record): {};
}
