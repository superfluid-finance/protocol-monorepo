// TODO: fs type, elements type for generator
export class GasMeterJSONReporter extends GasMeterReporter {
    generateOutput(report: { aggregates: {}, executedTxs: {}[] }): void;
}
export class GasMeterHTMLReporter extends GasMeterReporter {
    _joinStrings(prevVal: string, currVal: string, idx: number): string;
    _generateHeaders(element: any): string;
    _generateBody(elements: any): string;
    generateReport(report: { aggregates: {}, executedTxs: {}[] }): void;
}
declare class GasMeterReporter {
    constructor({ fileSystem, fileName }: {
        fileSystem: any;
        fileName: string;
    });
    fs: any;
    filePath: string;
    fileName: string;
}
export {};
