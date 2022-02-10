import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {EndpointBuilder} from '@reduxjs/toolkit/dist/query/endpointDefinitions';
import {BaseQueryFn, CreateApi} from '@reduxjs/toolkit/query';

import {getConfig} from '../../sdkReduxConfig';
import {typeGuard} from '../../utils';

import {CacheTagTypes} from './cacheTags/CacheTagTypes';
import {getSerializeQueryArgs} from './getSerializeQueryArgs';
import {MutationMeta, ValidationError} from './returnTypes';

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSfApiSlice = <T extends ModuleName>(createRtkQueryApi: CreateApi<T>) => {
    const slice = createRtkQueryApi({
        reducerPath: 'sfApi',
        baseQuery: apiSliceBaseQuery(),
        tagTypes: typeGuard<CacheTagTypes[]>(['Event', 'Index', 'Stream', 'Token']),
        endpoints: () => ({}),
        serializeQueryArgs: getSerializeQueryArgs(),
    });
    getConfig().setApiSlice(slice as any);
    return slice;
};

export type SfApiSliceInferredType = ReturnType<typeof initializeSfApiSlice>;

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
