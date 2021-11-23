import { SerializedError } from '@reduxjs/toolkit';
import { BaseQueryFn } from '@reduxjs/toolkit/dist/query/react';

import { QueryArg, SuperTokenMutationArg, TransactionInfo } from '../baseArg';

export const _NEVER = /* @__PURE__ */ Symbol();
export type NEVER = typeof _NEVER;

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

/**
 * Creates a "fake" baseQuery to be used if your api *only* uses the `queryFn` definition syntax.
 * This also allows you to specify a specific error type to be shared by all your `queryFn` definitions.
 */
export function rtkQuerySliceBaseQuery(): BaseQueryFn<
    SuperTokenMutationArg | QueryArg,
    TransactionInfo | Record<string, unknown>,
    ValidationError,
    unknown,
    MutationMeta
> {
    return function () {
        throw new Error(
            'All queries & mutations must use the `queryFn` definition syntax.'
        );
    };
}

export type PossibleErrors = ValidationError | SerializedError;
