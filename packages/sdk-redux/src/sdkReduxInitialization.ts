import {CreateApi} from '@reduxjs/toolkit/dist/query';
import type {ModuleName} from '@reduxjs/toolkit/dist/query/apiTypes';
import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

import {createRpcSlice} from './redux-slices/rtk-query/rpcSlice/rpcSlice';
import {createSubgraphSlice} from './redux-slices/rtk-query/subgraphSlice/subgraphSlice';
import {createTransactionSlice} from './redux-slices/transactionSlice/createTransactionSlice';
import {getConfig} from './sdkReduxConfig';

/**
 * For initializing "rpcSlice" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeRpcSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const rpcSlice = createRpcSlice(createApi);
    getConfig().setRpcSlice(rpcSlice as any);
    return rpcSlice;
};

/**
 * For initializing "sfApi" Redux slice.
 *
 * @param createApi Pass in either {@see createApiWithReactHooks} or {@see createApiWithoutReactHooks}.
 * You can wrap the function with your own function to add even more configuration to the RTK-Query API (e.g. "redux-persist" support).
 */
export const initializeSubgraphSlice = <T extends ModuleName>(createApi: CreateApi<T>) => {
    const subgraphSlice = createSubgraphSlice(createApi);
    getConfig().setSubgraphSlice(subgraphSlice as any);
    return subgraphSlice;
};

/**
 * For initializing "sfTransaction" Redux slice.
 */
export const initializeTransactionSlice = () => {
    const transactionSlice = createTransactionSlice();
    getConfig().setTransactionSlice(transactionSlice);
    return transactionSlice;
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
