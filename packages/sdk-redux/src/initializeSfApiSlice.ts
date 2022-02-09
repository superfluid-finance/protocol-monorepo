import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';

import {createApiSlice} from './redux-slices/rtk-query/sfApiSlice';
import {SfApiSliceInferredType} from './redux-slices/rtk-query/sfApiSliceInferredType';
import {getConfig} from './sdkReduxConfig';

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSfApiSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const slice = createApiSlice(createApi);
    getConfig().setApiSlice(slice as unknown as SfApiSliceInferredType);
    return slice;
};
