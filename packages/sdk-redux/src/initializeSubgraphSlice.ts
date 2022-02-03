import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';

import {SfSubgraphSliceInferredType} from './redux-slices/rtk-query/subgraph-slice/sfSubgraphSliceInferredType';
import {createSubgraphSlice} from './redux-slices/rtk-query/subgraph-slice/subgraphSlice';
import {getConfig} from './sdkReduxConfig';

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSubgraphSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const slice = createSubgraphSlice(createApi);
    getConfig().setSubgraphSlice(slice as unknown as SfSubgraphSliceInferredType);
    return slice;
};
