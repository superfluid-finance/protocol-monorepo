import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';
import _ from 'lodash';

// NOTE: This file is marked for side-effects inside the package.json for efficient tree-shaking.

import {RpcApiSliceEmpty} from './reduxSlices/rtkQuery/rpcApiSlice/rpcApiSlice';
import {SubgraphApiSliceEmpty} from './reduxSlices/rtkQuery/subgraphApiSlice/subgraphApiSlice';
import {TransactionSlice} from './reduxSlices/transactionSlice/createTransactionSlice';

interface FrameworkLocator {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (chainId: number, frameworkOrFactory: Framework | (() => Promise<Framework>)) => void;
}

interface SignerLocator {
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (chainId: number, signerOrFactory: Signer | (() => Promise<Signer>)) => void;
}

interface RpcApiSliceLocator {
    getRpcApiSlice: () => RpcApiSliceEmpty;
    setRpcApiSlice: (api: RpcApiSliceEmpty) => void;
}

interface SubgraphApiSliceLocator {
    getSubgraphApiSlice: () => SubgraphApiSliceEmpty;
    setSubgraphApiSlice: (api: SubgraphApiSliceEmpty) => void;
}

interface TransactionSliceLocator {
    getTransactionSlice: () => TransactionSlice;
    setTransactionSlice: (slice: TransactionSlice) => void;
}

/**
 * NOTE: The reason memoization is used is to avoid multiple instantiations by the factory functions.
 */
export default class SdkReduxConfig
    implements FrameworkLocator, SignerLocator, RpcApiSliceLocator, SubgraphApiSliceLocator, TransactionSliceLocator
{
    rpcApiSlice: RpcApiSliceEmpty | undefined;
    subgraphApiSlice: SubgraphApiSliceEmpty | undefined;
    transactionSlice: TransactionSlice | undefined;
    memoizedFrameworkFactories = new Map<number, () => Promise<Framework>>();
    memoizedSignerFactories = new Map<number, () => Promise<Signer>>();

    static getOrCreateSingleton(): SdkReduxConfig {
        if (!globalThis.sdkReduxConfig) {
            globalThis.sdkReduxConfig = new SdkReduxConfig();
        }
        return globalThis.sdkReduxConfig;
    }

    getRpcApiSlice(): RpcApiSliceEmpty {
        if (!this.rpcApiSlice) {
            throw Error('The RpcApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.rpcApiSlice;
    }

    getSubgraphApiSlice(): SubgraphApiSliceEmpty {
        if (!this.subgraphApiSlice) {
            throw Error('The SubgraphApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.subgraphApiSlice;
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
            throw Error('The TransactionSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }
        return this.transactionSlice;
    }

    setRpcApiSlice(slice: RpcApiSliceEmpty): void {
        if (this.rpcApiSlice) {
            console.log(
                "Warning! RpcApiSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.rpcApiSlice = slice;
    }

    setSubgraphApiSlice(slice: SubgraphApiSliceEmpty): void {
        if (this.subgraphApiSlice) {
            console.log(
                "Warning! SubgraphApiSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.subgraphApiSlice = slice;
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
export const getRpcApiSlice = () => getConfig().getRpcApiSlice();
export const getSubgraphApiSlice = () => getConfig().getSubgraphApiSlice();
export const getTransactionSlice = () => getConfig().getTransactionSlice();
export const getFramework = (chainId: number) => getConfig().getFramework(chainId);
export const getSubgraphClient = (chainId: number) =>
    getConfig()
        .getFramework(chainId)
        .then((x) => x.query.subgraphClient);
export const getSigner = (chainId: number) => getConfig().getSigner(chainId);

const isEthersSigner = (value: any): value is Signer => !!value.getAddress;
const isFramework = (value: any): value is Framework => !!value.cfaV1;
