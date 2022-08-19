import { serializeError } from "serialize-error";

export type ErrorType =
    | "FRAMEWORK_INITIALIZATION"
    | "SUPERTOKEN_INITIALIZATION"
    | "CREATE_SIGNER"
    | "SUPERTOKEN_READ"
    | "SUBGRAPH_ERROR"
    | "CFAV1_READ"
    | "IDAV1_READ"
    | "INVALID_ADDRESS"
    | "INVALID_OBJECT"
    | "UNCLEAN_PERMISSIONS"
    | "NEGATIVE_FLOW_ALLOWANCE"
    | "EXECUTE_TRANSACTION"
    | "POPULATE_TRANSACTION"
    | "SIGN_TRANSACTION"
    | "UNSUPPORTED_OPERATION"
    | "MISSING_TRANSACTION_PROPERTIES"
    | "BATCH_CALL_ERROR"
    | "NETWORK_MISMATCH";

const errorTypeToTitleMap = new Map<ErrorType, string>([
    ["FRAMEWORK_INITIALIZATION", "Framework Initialization"],
    ["SUPERTOKEN_INITIALIZATION", "SuperToken Initialization"],
    ["CREATE_SIGNER", "Create Signer"],
    ["SUPERTOKEN_READ", "SuperToken Read"],
    ["SUBGRAPH_ERROR", "Subgraph"],
    ["CFAV1_READ", "ConstantFlowAgreementV1 Read"],
    ["IDAV1_READ", "InstantDistributionAgreementV1 Read"],
    ["INVALID_ADDRESS", "Invalid Address"],
    ["INVALID_OBJECT", "Invalid Object"],
    ["POPULATE_TRANSACTION", "Populate Transaction"],
    ["EXECUTE_TRANSACTION", "Execute Transaction"],
    ["SIGN_TRANSACTION", "Sign Transaction"],
    ["UNSUPPORTED_OPERATION", "Unsupported Batch Call Operation"],
    ["MISSING_TRANSACTION_PROPERTIES", "Missing Transaction Properties"],
    ["BATCH_CALL_ERROR", "Batch Call"],
    ["NETWORK_MISMATCH", "Network Mismatch"],
    ["UNCLEAN_PERMISSIONS", "Unclean Permissions"],
    ["NEGATIVE_FLOW_ALLOWANCE", "Negative Flow Rate Allowance"],
]);

interface ErrorProps {
    type: ErrorType;
    message: string;
    cause?: Error | unknown;
}

// NOTE: this is a temporary solution to fix serializeError
// which throws a weird JSON error
const stringifyCause = (cause?: Error | unknown) => {
    try {
        return JSON.stringify(serializeError(cause), null, 2);
    } catch (err) {
        try {
            return JSON.stringify(cause, null, 2);
        } catch {
            console.error("Caused by: ", cause);
            return "[Couldn't serialize error. Error logged to console.]";
        }
    }
};

export class SFError extends Error {
    readonly type: ErrorType;
    override readonly cause?: Error;

    constructor({ type, message, cause }: ErrorProps) {
        const fullMessage = `${errorTypeToTitleMap.get(
            type
        )} Error: ${message}${
            cause
                ? `
Caused by: ${stringifyCause(cause)}`
                : ""
        }`;
        super(
            fullMessage,
            cause
                ? {
                      cause: cause as Error, // Currently "unknown" is not compatible with "cause" (because it expects "Error" and that's why we cast) but this was recently changed and merged to also allow "unknown": https://github.com/microsoft/TypeScript/pull/49639
                  }
                : {}
        );
        // Fallback back environments where `Error.cause` is now yet natively supported
        if (cause && !this.cause) {
            this.cause = cause as Error;
        }
        this.type = type;
        Object.setPrototypeOf(this, new.target.prototype); // restore prototype chain: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#support-for-newtarget
    }
}
