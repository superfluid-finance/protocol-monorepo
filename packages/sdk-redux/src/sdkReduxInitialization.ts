import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

import {createApiSlice} from './redux-slices/rtk-query/sfApiSlice';
import {createSubgraphSlice} from './redux-slices/rtk-query/subgraph-slice/subgraphSlice';
import {createTransactionSlice} from './redux-slices/transactions/createTransactionSlice';
import {getConfig} from './sdkReduxConfig';

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSfApiSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const slice = createApiSlice(createApi);
    getConfig().setApiSlice(slice as any);
    return {sfApi: slice};
};

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSubgraphSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const slice = createSubgraphSlice(createApi);
    getConfig().setSubgraphSlice(slice as any);
    return slice;
};

/**
 * For initializing "sfTransaction" Redux slice.
 */
export const initializeSfTransactionSlice = () => {
    const slice = createTransactionSlice();
    getConfig().setTransactionSlice(slice);
    return {sfTransactions: slice};
};

/**
 * SDK-Redux needs to know where to get the signer for mutations (i.e. transaction signing & broadcasting)
 * @param chainId The chain.
 * @param signer The Ethers signer getter. Can be either Promise or instance.
 */
export const setSignerForSdkRedux = (chainId: number, signer: (() => Promise<Signer>) | Signer) =>
    getConfig().setSigner(chainId, signer);

/**
 * SDK-Redux needs to know where to get SDK-Core's Framework for data querying.
 * @param chainId The chain.
 * @param framework The SDK-Framework getter. Can be either Promise or instance.
 */
export const setFrameworkForSdkRedux = (chainId: number, framework: (() => Promise<Framework>) | Framework) =>
    getConfig().setFramework(chainId, framework);
