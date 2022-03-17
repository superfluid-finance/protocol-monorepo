import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';
import _ from 'lodash';

import {RpcApiEmpty} from './redux-slices/rtk-query/rpcApi/rpcApi';
import {SubgraphApiEmpty} from './redux-slices/rtk-query/subgraphApi/subgraphApi';
import {SfTransactionSliceType} from './redux-slices/transactions/createTransactionSlice';

interface FrameworkLocator {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (chainId: number, frameworkOrFactory: Framework | (() => Promise<Framework>)) => void;
}

interface SignerLocator {
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (chainId: number, signerOrFactory: Signer | (() => Promise<Signer>)) => void;
}

interface RpcApiLocator {
    getRpcApi: () => RpcApiEmpty;
    setRpcApi: (api: RpcApiEmpty) => void;
}

interface SubgraphSliceLocator {
    getSubgraphSlice: () => SubgraphApiEmpty;
    setSubgraphSlice: (slice: SubgraphApiEmpty) => void;
}

interface TransactionSliceLocator {
    getTransactionSlice: () => SfTransactionSliceType;
    setTransactionSlice: (slice: SfTransactionSliceType) => void;
}

/**
 * NOTE: The reason memoization is used is to avoid multiple instantiations by the factory functions.
 */
export default class SdkReduxConfig
    implements FrameworkLocator, SignerLocator, RpcApiLocator, SubgraphSliceLocator, TransactionSliceLocator
{
    apiSlice: RpcApiEmpty | undefined;
    subgraphSlice: SubgraphApiEmpty | undefined;
    transactionSlice: SfTransactionSliceType | undefined;
    memoizedFrameworkFactories = new Map<number, () => Promise<Framework>>();
    memoizedSignerFactories = new Map<number, () => Promise<Signer>>();

    static getOrCreateSingleton(): SdkReduxConfig {
        if (!globalThis.sdkReduxConfig) {
            globalThis.sdkReduxConfig = new SdkReduxConfig();
        }
        return globalThis.sdkReduxConfig;
    }

    getRpcApi(): RpcApiEmpty {
        if (!this.apiSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.apiSlice;
    }

    getSubgraphSlice(): SubgraphApiEmpty {
        if (!this.subgraphSlice) {
            throw Error('The SubgraphSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.subgraphSlice;
    }

    getFramework(chainId: number): Promise<Framework> {
        const frameworkFactory = this.memoizedFrameworkFactories.get(chainId);
        if (!frameworkFactory)
            throw Error(
                `Don't know how to get Superfluid Framework. :( Please set up a *framework* source for chain [${chainId}].`
            );
        return frameworkFactory();
    }

    getSigner(chainId: number): Promise<Signer> {
        const signerFactory = this.memoizedSignerFactories.get(chainId);
        if (!signerFactory)
            throw Error(`Don't know how to get a signer. :( Please set up a *signer* source for chain [${chainId}].`);
        return signerFactory();
    }

    getTransactionSlice(): SfTransactionSliceType {
        if (!this.transactionSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.transactionSlice;
    }

    setRpcApi(slice: RpcApiEmpty): void {
        if (this.apiSlice) {
            console.log(
                "Warning! ApiAlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.apiSlice = slice;
    }

    setSubgraphSlice(slice: SubgraphApiEmpty): void {
        if (this.subgraphSlice) {
            console.log(
                "Warning! SubgraphSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.subgraphSlice = slice;
    }

    setFramework(chainId: number, instanceOrFactory: Framework | (() => Promise<Framework>)) {
        const frameworkFactory = isFramework(instanceOrFactory)
            ? () => Promise.resolve(instanceOrFactory)
            : instanceOrFactory;

        this.memoizedFrameworkFactories.set(chainId, _.memoize(frameworkFactory));
    }

    setSigner(chainId: number, instanceOrFactory: Signer | (() => Promise<Signer>)) {
        const signerFactory = isEthersSigner(instanceOrFactory)
            ? () => Promise.resolve(instanceOrFactory)
            : instanceOrFactory;

        this.memoizedSignerFactories.set(chainId, _.memoize(signerFactory));
    }

    setTransactionSlice(slice: SfTransactionSliceType): void {
        if (this.transactionSlice) {
            console.log(
                "Warning! TransactionSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.transactionSlice = slice;
    }
}

export const getConfig = SdkReduxConfig.getOrCreateSingleton;
export const getApiSlice = () => getConfig().getRpcApi();
export const getSubgraphSlice = () => getConfig().getSubgraphSlice();
export const getTransactionSlice = () => getConfig().getTransactionSlice();
export const getFramework = (chainId: number) => getConfig().getFramework(chainId);
export const getSigner = (chainId: number) => getConfig().getSigner(chainId);

const isEthersSigner = (value: any): value is Signer => !!value.getAddress;
const isFramework = (value: any): value is Framework => !!value.cfaV1;
