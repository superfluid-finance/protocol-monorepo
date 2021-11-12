import { BaseQueryFn } from '@reduxjs/toolkit/dist/query/react';
import { SerializedError } from '@reduxjs/toolkit';

import { MutationArg, QueryArg, TransactionInfo } from '../baseArg';

export const _NEVER = /* @__PURE__ */ Symbol();
export type NEVER = typeof _NEVER;

export interface ValidationError {
    message: string; // Keep it named "message" to have same structure with Redux Toolkit's SerializedError.
}

/**
 * Creates a "fake" baseQuery to be used if your api *only* uses the `queryFn` definition syntax.
 * This also allows you to specify a specific error type to be shared by all your `queryFn` definitions.
 */
export function rtkQuerySliceBaseQuery(): BaseQueryFn<
    MutationArg | QueryArg,
    TransactionInfo | Record<string, unknown>,
    ValidationError,
    unknown
> {
    return function () {
        throw new Error(
            'All queries & mutations must use the `queryFn` definition syntax.'
        );
    };
}

export type PossibleErrors = ValidationError | SerializedError;
