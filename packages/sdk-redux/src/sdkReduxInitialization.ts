import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {createApi as createApiWithoutReactHooks} from '@reduxjs/toolkit/query';
import {createApi as createApiWithReactHooks} from '@reduxjs/toolkit/query/react';

import {getConfig} from './sdkReduxConfig';
import {createApiSlice} from './redux-slices/rtk-query/sfApiSlice';
import {createTransactionSlice} from './redux-slices/transactions/createTransactionSlice';
import { Signer } from 'ethers';
import { Framework } from '@superfluid-finance/sdk-core';

export {createApiWithoutReactHooks};
export {createApiWithReactHooks};

export const initializeSfApiSlice = <T extends ModuleName>(createApi: CreateApi<T> = createApiWithReactHooks) => {
    const slice = createApiSlice(createApi);
    getConfig().setApiSlice(slice as any);
    return {sfApi: slice};
};

export const initializeSfTransactionSlice = () => {
    const slice = createTransactionSlice();
    getConfig().setTransactionSlice(slice);
    return {sfTransactions: slice};
};

export const setSignerForSdkRedux = (chainId: number, signer: (() => Promise<Signer>) | Signer) => getConfig().setSigner(chainId, signer)
export const setFrameworkForSdkRedux = (chainId: number, framework: (() => Promise<Framework>) | Framework) => getConfig().setFramework(chainId, framework)
