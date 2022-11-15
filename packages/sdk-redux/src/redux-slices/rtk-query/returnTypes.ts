import {SerializedError} from '@reduxjs/toolkit';
import {ErrorType, SFError} from '@superfluid-finance/sdk-core';

export type ValidationError = {
    /**
     * NOTE: Keep it named "message" to have same structure with Redux Toolkit's SerializedError.
     */
    message: string;
};

/**
 * Effectively {@see SFError} serialized because Redux can't store class instances.
 */
export type SerializedSFError = {
    /**
     * NOTE: Keep it named "message" to have same structure with Redux Toolkit's SerializedError.
     */
    message: string;
    type: ErrorType;
    errorObject?: SerializedError;
};

export type MutationMeta = {
    /**
     * The address will be monitored for events to invalidate cache.
     */
    monitorAddress: string;
};

export type PossibleErrors = ValidationError | SerializedError | SFError;
