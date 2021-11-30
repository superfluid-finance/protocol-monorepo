/**
 * Information about the transaction to locate it.
 */
export type TransactionInfo = {
    /** The transaction's network chain ID. */
    chainId: number;
    /** The transaction hash. */
    hash: string;
};

/**
 *
 */
export type QueryArg = {
    /** Queried network's chain ID. */
    chainId: number;
};

/**
 *
 */
export type PaginatedQueryArg = QueryArg & {
    /** How many entries to skip. */
    skip: number;
    /** How many entries to return. */
    take: number;
};

/**
 * TEST
 */
export type SuperTokenMutationArg = {
    /**  The network's chain ID where transactions will be broadcast. */
    chainId: number;
    /** SuperToken the mutation will affect. */
    superTokenAddress: string;
    /**
     * Whether to wait for one confirmation to the transaction or not.
     *
     * If set to `true` then the mutation will be blocking, i.e. the react hook / redux thunk will return control flow after first confirmation for the transaction broadcast.
     * If set to `false` then the mutation will not be blocking, i.e. the react hook / redux thunk will not return.
     */
    waitForConfirmation: boolean | NothingBoolean;
};

/**
 * Input for strings that is ignored.
 */
export type NothingString = '' | undefined;

/**
 * Input for numbers that is ignored.
 */
export type NothingNumber = undefined;

/**
 * Input for booleans that is ignored.
 */
export type NothingBoolean = undefined;
