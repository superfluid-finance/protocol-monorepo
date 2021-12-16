import {SerializedError} from '@reduxjs/toolkit';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {BaseQueryFn} from '@reduxjs/toolkit/dist/query/react';

import {BaseQuery, BaseSuperTokenMutation, TransactionInfo} from '../argTypes';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';

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
export function baseQuery(): BaseQueryFn<
    BaseSuperTokenMutation | BaseQuery<unknown>,
    TransactionInfo | Record<string, unknown>,
    ValidationError,
    unknown,
    MutationMeta
> {
    return function () {
        throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
    };
}

export type PossibleErrors = ValidationError | SerializedError;

export type SfEndpointBuilder = EndpointBuilder<
    BaseQueryFn<
        BaseSuperTokenMutation | BaseQuery<unknown>,
        TransactionInfo | Record<string, unknown>,
        ValidationError,
        unknown,
        MutationMeta
    >,
    CacheTagTypes,
    'sfApi'
>;
