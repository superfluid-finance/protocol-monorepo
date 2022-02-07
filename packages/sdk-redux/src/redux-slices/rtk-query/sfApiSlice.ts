import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {BaseQueryFn, CreateApi} from '@reduxjs/toolkit/query';

import {typeGuard} from '../../utils';

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
    void,
    unknown,
    ValidationError,
    Record<string, unknown>,
    MutationMeta
> {
    return function () {
        throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
    };
}

export type ApiSliceEndpointBuilder = EndpointBuilder<ReturnType<typeof apiSliceBaseQuery>, CacheTagTypes, 'sfApi'>;
