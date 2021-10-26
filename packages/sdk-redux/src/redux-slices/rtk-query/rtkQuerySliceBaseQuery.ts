import { BaseQueryFn } from '@reduxjs/toolkit/dist/query/react';

export const _NEVER = /* @__PURE__ */ Symbol();
export type NEVER = typeof _NEVER;

export interface Error {
    message: string;
}

/**
 * Creates a "fake" baseQuery to be used if your api *only* uses the `queryFn` definition syntax.
 * This also allows you to specify a specific error type to be shared by all your `queryFn` definitions.
 */
export function rtkQuerySliceBaseQuery(): BaseQueryFn<
    void,
    unknown,
    Error,
    unknown
> {
    return function () {
        throw new Error(
            'All queries & mutations must use the `queryFn` definition syntax.'
        );
    };
}
