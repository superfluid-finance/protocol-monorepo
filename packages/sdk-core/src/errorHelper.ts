type ErrorType =
    | "FRAMEWORK_INITIALIZATION"
    | "SUPERTOKEN_READ"
    | "SUPERTOKEN_WRITE"
    | "SUBGRAPH_ERROR"
    | "CFAV1_READ"
    | "CFAV1_WRITE"
    | "IDAV1_READ"
    | "IDAV1_WRITE"
    | "INVALID_ADDRESS"
    | "POPULATE_TRANSACTION";

const errorTypeToTitleMap = new Map<ErrorType, string>([
    ["FRAMEWORK_INITIALIZATION", "Framework Initialization"],
    ["SUPERTOKEN_READ", "SuperToken Read"],
    ["SUPERTOKEN_WRITE", "SuperToken Write"],
    ["SUBGRAPH_ERROR", "Subgraph"],
    ["CFAV1_READ", "ConstantFlowAgreementV1 Read"],
    ["CFAV1_WRITE", "ConstantFlowAgreementV1 Write"],
    ["IDAV1_READ", "InstantDistributionAgreementV1 Read"],
    ["IDAV1_WRITE", "InstantDistributionAgreementV1 Write"],
    ["INVALID_ADDRESS", "Invalid Address"],
    ["POPULATE_TRANSACTION", "Populate Transaction"],
]);

export const handleError = (errorType: ErrorType, text: string, errObject?: string) => {
    let errorTitle = errorTypeToTitleMap.get(errorType);
    if (!errorTitle) {
        errorTitle = "Unknown Error";
    }
    const errObjectFormatted = errObject ? "\n" + errObject : "";
    throw new Error(errorTitle + " Error - " + text + errObjectFormatted,);
};
