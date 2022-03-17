import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';
import _ from 'lodash';

// NOTE: This file is marked for side-effects inside the package.json for efficient tree-shaking.

import {RpcSliceEmpty} from './reduxSlices/rtkQuery/rpcSlice/rpcSlice';
import {SubgraphSliceEmpty} from './reduxSlices/rtkQuery/subgraphSlice/subgraphSlice';
import {TransactionSlice} from './reduxSlices/transactionSlice/createTransactionSlice';

interface FrameworkLocator {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (chainId: number, frameworkOrFactory: Framework | (() => Promise<Framework>)) => void;
}

interface SignerLocator {
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (chainId: number, signerOrFactory: Signer | (() => Promise<Signer>)) => void;
}

interface RpcSliceLocator {
    getRpcSlice: () => RpcSliceEmpty;
    setRpcSlice: (api: RpcSliceEmpty) => void;
}

interface SubgraphSliceLocator {
    getSubgraphSlice: () => SubgraphSliceEmpty;
    setSubgraphSlice: (api: SubgraphSliceEmpty) => void;
}

interface TransactionSliceLocator {
    getTransactionSlice: () => TransactionSlice;
    setTransactionSlice: (slice: TransactionSlice) => void;
}

/**
 * NOTE: The reason memoization is used is to avoid multiple instantiations by the factory functions.
 */
export default class SdkReduxConfig
    implements FrameworkLocator, SignerLocator, RpcSliceLocator, SubgraphSliceLocator, TransactionSliceLocator
{
    rpcSlice: RpcSliceEmpty | undefined;
    subgraphSlice: SubgraphSliceEmpty | undefined;
    transactionSlice: TransactionSlice | undefined;
    memoizedFrameworkFactories = new Map<number, () => Promise<Framework>>();
    memoizedSignerFactories = new Map<number, () => Promise<Signer>>();

    static getOrCreateSingleton(): SdkReduxConfig {
        if (!globalThis.sdkReduxConfig) {
            globalThis.sdkReduxConfig = new SdkReduxConfig();
        }
        return globalThis.sdkReduxConfig;
    }

    getRpcSlice(): RpcSliceEmpty {
        if (!this.rpcSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.rpcSlice;
    }

    getSubgraphSlice(): SubgraphSliceEmpty {
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

    getTransactionSlice(): TransactionSlice {
        if (!this.transactionSlice) {
            throw Error('The RpcSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.transactionSlice;
    }

    setRpcSlice(slice: RpcSliceEmpty): void {
        if (this.rpcSlice) {
            console.log(
                "Warning! RpcSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.rpcSlice = slice;
    }

    setSubgraphSlice(slice: SubgraphSliceEmpty): void {
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

    setTransactionSlice(slice: TransactionSlice): void {
        if (this.transactionSlice) {
            console.log(
                "Warning! TransactionSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.transactionSlice = slice;
    }
}

export const getConfig = SdkReduxConfig.getOrCreateSingleton;
export const getRpcSlice = () => getConfig().getRpcSlice();
export const getSubgraphSlice = () => getConfig().getSubgraphSlice();
export const getTransactionSlice = () => getConfig().getTransactionSlice();
export const getFramework = (chainId: number) => getConfig().getFramework(chainId);
export const getSigner = (chainId: number) => getConfig().getSigner(chainId);

const isEthersSigner = (value: any): value is Signer => !!value.getAddress;
const isFramework = (value: any): value is Framework => !!value.cfaV1;
