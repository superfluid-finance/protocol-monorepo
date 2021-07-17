export class GasMeterJSONReporter extends GasMeterReporter {
    generateOutput(report: any): void;
}
export class GasMeterHTMLReporter extends GasMeterReporter {
    _joinStrings(prevVal: any, currVal: any, idx: any): any;
    _generateHeaders(element: any): string;
    _generateBody(elements: any): any;
    generateReport(report: any): void;
}
declare class GasMeterReporter {
    constructor({ fileSystem, fileName }: {
        fileSystem: any;
        fileName: any;
    });
    fs: any;
    filePath: string;
    fileName: any;
}
export {};
