import {SerializedError} from '@reduxjs/toolkit';
import {SFError} from '@superfluid-finance/sdk-core';

export type ValidationError = {
    /**
     * NOTE: Keep it named "message" to have same structure with Redux Toolkit's SerializedError.
     */
    message: string;
};

export type MutationMeta = {
    /**
     * The address will be monitored for events to invalidate cache.
     */
    monitorAddress: string;
};

export type PossibleErrors = ValidationError | SerializedError | SFError;
