import { miniSerializeError } from "./miniSerializeError";

export type ErrorType =
    | "FRAMEWORK_INITIALIZATION"
    | "SUPERTOKEN_INITIALIZATION"
    | "CREATE_SIGNER"
    | "SUPERTOKEN_READ"
    | "CFAV1_READ"
    | "NFT_READ"
    | "IDAV1_READ"
    | "GDAV1_READ"
    | "SUPERFLUID_POOL_READ"
    | "GDAV1_WRITE"
    | "INVALID_ADDRESS"
    | "INVALID_OBJECT"
    | "UNCLEAN_PERMISSIONS"
    | "NEGATIVE_FLOW_ALLOWANCE"
    | "UNSUPPORTED_OPERATION"
    | "MISSING_TRANSACTION_PROPERTIES"
    | "BATCH_CALL_ERROR"
    | "NETWORK_MISMATCH";

const errorTypeToTitleMap = new Map<ErrorType, string>([
    ["FRAMEWORK_INITIALIZATION", "Framework Initialization"],
    ["SUPERTOKEN_INITIALIZATION", "SuperToken Initialization"],
    ["CREATE_SIGNER", "Create Signer"],
    ["SUPERTOKEN_READ", "SuperToken Read"],
    ["CFAV1_READ", "ConstantFlowAgreementV1 Read"],
    ["IDAV1_READ", "InstantDistributionAgreementV1 Read"],
    ["GDAV1_READ", "GeneralDistributionAgreementV1 Read"],
    ["GDAV1_WRITE", "GeneralDistributionAgreementV1 Write"],
    ["SUPERFLUID_POOL_READ", "Superfluid Pool Read"],
    ["INVALID_ADDRESS", "Invalid Address"],
    ["INVALID_OBJECT", "Invalid Object"],
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

const miniStringifyCause = (cause?: Error | unknown) => {
    try {
        const serializedError = miniSerializeError(cause);
        const stringifiedError = JSON.stringify(serializedError, null, 2);
        return stringifiedError.replace(/\\"/g, '"'); // Get rid of escaping of quotes.
    } catch {
        // `miniSerializeError` is safe enough that this should never occur.
        console.error("SFError caused by: ", cause);
        return "[Couldn't serialize internal error. Error logged to console instead.]";
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
Caused by: ${miniStringifyCause(cause)}`
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
        this.name = "SFError";
        Object.setPrototypeOf(this, new.target.prototype); // restore prototype chain: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-2.html#support-for-newtarget
    }
}
