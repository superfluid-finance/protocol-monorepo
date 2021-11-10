type ErrorType =
    | "FRAMEWORK_INITIALIZATION"
    | "SUPERTOKEN_INITIALIZATION"
    | "CREATE_SIGNER"
    | "SUPERTOKEN_READ"
    | "SUPERTOKEN_WRITE"
    | "SUBGRAPH_ERROR"
    | "CFAV1_READ"
    | "CFAV1_WRITE"
    | "IDAV1_READ"
    | "IDAV1_WRITE"
    | "INVALID_ADDRESS"
    | "INVALID_OBJECT"
    | "EXECUTE_TRANSACTION"
    | "POPULATE_TRANSACTION"
    | "SIGN_TRANSACTION"
    | "GET_TRANSACTION_HASH"
    | "UNSUPPORTED_OPERATION"
    | "MISSING_TRANSACTION_PROPERTIES"
    | "BATCH_CALL_ERROR";

const errorTypeToTitleMap = new Map<ErrorType, string>([
    ["FRAMEWORK_INITIALIZATION", "Framework Initialization"],
    ["SUPERTOKEN_INITIALIZATION", "SuperToken Initialization"],
    ["CREATE_SIGNER", "Create Signer"],
    ["SUPERTOKEN_READ", "SuperToken Read"],
    ["SUPERTOKEN_WRITE", "SuperToken Write"],
    ["SUBGRAPH_ERROR", "Subgraph"],
    ["CFAV1_READ", "ConstantFlowAgreementV1 Read"],
    ["CFAV1_WRITE", "ConstantFlowAgreementV1 Write"],
    ["IDAV1_READ", "InstantDistributionAgreementV1 Read"],
    ["IDAV1_WRITE", "InstantDistributionAgreementV1 Write"],
    ["INVALID_ADDRESS", "Invalid Address"],
    ["INVALID_OBJECT", "Invalid Object"],
    ["POPULATE_TRANSACTION", "Populate Transaction"],
    ["EXECUTE_TRANSACTION", "Execute Transaction"],
    ["SIGN_TRANSACTION", "Sign Transaction"],
    ["GET_TRANSACTION_HASH", "Get Transaction Hash"],
    ["UNSUPPORTED_OPERATION", "Unsupported Batch Call Operation"],
    ["MISSING_TRANSACTION_PROPERTIES", "Missing Transaction Properties"],
    ["BATCH_CALL_ERROR", "Batch Call Error"]
]);

export const handleError = (
    errorType: ErrorType,
    text: string,
    errObject?: string
) => {
    let errorTitle = errorTypeToTitleMap.get(errorType);
    if (!errorTitle) {
        errorTitle = "Unknown Error";
    }
    const errObjectFormatted = errObject ? ": \n" + errObject : "";
    throw new Error(errorTitle + " Error - " + text + errObjectFormatted);
};
