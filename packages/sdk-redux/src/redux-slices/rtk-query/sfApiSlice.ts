import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {BaseQueryFn, CreateApi} from '@reduxjs/toolkit/query';

import {typeGuard} from '../../utils';
import {BaseQuery, BaseSuperTokenMutation, TransactionInfo} from '../argTypes';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';
import {getSerializeQueryArgs} from './getSerializeQueryArgs';
import {addMutationEndpoints} from './mutations/addMutationEndpoints';
import {addQueryEndpoints} from './queries/addQueryEndpoints';
import {MutationMeta, ValidationError} from './returnTypes';

export const createApiSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) =>
    createRtkQueryApi({
        reducerPath: 'sfApi',
        baseQuery: apiSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: (builder) => ({
            ...addQueryEndpoints(builder),
            ...addMutationEndpoints(builder),
        }),
        serializeQueryArgs: getSerializeQueryArgs(),
    });

export function apiSliceBaseQuery(): BaseQueryFn<
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

export type ApiSliceEndpointBuilder = EndpointBuilder<
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

// NOTE: This might not include all the type info.
export type SfApiSliceInferredType = ReturnType<typeof createApiSlice>;
