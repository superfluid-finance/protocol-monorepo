import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {createApi as createApiWithoutReactHooks} from '@reduxjs/toolkit/query';
import {createApi as createApiWithReactHooks} from '@reduxjs/toolkit/query/react';

import {getSuperfluidContext} from './SuperfluidContext';
import {createApiSlice} from './redux-slices/rtk-query/sfApiSlice';
import {createTransactionSlice} from './redux-slices/transactions/createTransactionSlice';

export {createApiWithoutReactHooks};
export {createApiWithReactHooks};

export const initializeSfApiSlice = <T extends ModuleName>(createApi: CreateApi<T> = createApiWithReactHooks) => {
    const slice = createApiSlice(createApi);
    getSuperfluidContext().setApiSlice(slice as any);
    return {sfApi: slice};
};

export const initializeSfTransactionSlice = () => {
    const slice = createTransactionSlice();
    getSuperfluidContext().setTransactionSlice(slice);
    return {sfTransactions: slice};
};
