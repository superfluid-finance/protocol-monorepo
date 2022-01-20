import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

import {SfApiSliceInferredType} from './redux-slices/rtk-query/sfApiSliceInferredType';
import {SfSubgraphSliceInferredType} from './redux-slices/rtk-query/sfSubgraphSliceInferredType';
import {SfTransactionSliceType} from './redux-slices/transactions/createTransactionSlice';

interface FrameworkLocator {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (chainId: number, framework: (() => Promise<Framework>) | Framework) => void;
}

interface SignerLocator {
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (chainId: number, signer: (() => Promise<Signer>) | Signer) => void;
}

interface FrameworkAndSignerLocator extends FrameworkLocator, SignerLocator {
    getFrameworkAndSigner: (chainId: number) => Promise<[framework: Framework, signer: Signer]>;
}

interface ApiSliceLocator {
    getApiSlice: () => SfApiSliceInferredType;
    setApiSlice: (slice: SfApiSliceInferredType) => void;
}

interface SubgraphSliceLocator {
    getSubgraphSlice: () => SfSubgraphSliceInferredType;
    setSubgraphSlice: (slice: SfSubgraphSliceInferredType) => void;
}

interface TransactionSliceLocator {
    getTransactionSlice: () => SfTransactionSliceType;
    setTransactionSlice: (slice: SfTransactionSliceType) => void;
}

export default class SdkReduxConfig
    implements FrameworkAndSignerLocator, ApiSliceLocator, SubgraphSliceLocator, TransactionSliceLocator
{
    apiSlice: SfApiSliceInferredType | undefined;
    subgraphSlice: SfSubgraphSliceInferredType | undefined;
    transactionSlice: SfTransactionSliceType | undefined;
    frameworks = new Map<number, () => Promise<Framework>>();
    signers = new Map<number, () => Promise<Signer>>();

    static getOrCreateSingleton(): SdkReduxConfig {
        if (!globalThis.sdkReduxConfig) {
            globalThis.sdkReduxConfig = new SdkReduxConfig();
        }
        return globalThis.sdkReduxConfig;
    }

    getApiSlice(): SfApiSliceInferredType {
        if (!this.apiSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }

        return this.apiSlice;
    }

    getSubgraphSlice(): SfSubgraphSliceInferredType {
        if (!this.subgraphSlice) {
            throw Error('The SubgraphSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }

        return this.subgraphSlice;
    }

    getFramework(chainId: number): Promise<Framework> {
        const frameworkGetter = this.frameworks.get(chainId);
        if (!frameworkGetter)
            throw Error(
                `Don't know how to get Superfluid Framework. :( Please set up a *framework* source for chain [${chainId}].`
            );
        return frameworkGetter();
    }

    getSigner(chainId: number): Promise<Signer> {
        const signerGetter = this.signers.get(chainId);
        if (!signerGetter)
            throw Error(`Don't know how to get a signer. :( Please set up a *signer* source for chain [${chainId}].`);
        return signerGetter();
    }

    getTransactionSlice(): SfTransactionSliceType {
        if (!this.transactionSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }

        return this.transactionSlice;
    }

    setApiSlice(slice: SfApiSliceInferredType): void {
        if (this.apiSlice) {
            console.log(
                "Warning! ApiAlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.apiSlice = slice;
    }

    setSubgraphSlice(slice: SfSubgraphSliceInferredType): void {
        if (this.subgraphSlice) {
            console.log(
                "Warning! SubgraphSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.subgraphSlice = slice;
    }

    setFramework(chainId: number, framework: (() => Promise<Framework>) | Framework) {
        if (framework instanceof Framework) {
            this.frameworks.set(chainId, () => Promise.resolve(framework));
        } else {
            this.frameworks.set(chainId, framework);
        }
    }

    setSigner(chainId: number, signer: (() => Promise<Signer>) | Signer) {
        if (isEthersSigner(signer)) {
            this.signers.set(chainId, () => Promise.resolve(signer));
        } else {
            this.signers.set(chainId, signer);
        }
    }

    setTransactionSlice(slice: SfTransactionSliceType): void {
        if (this.transactionSlice) {
            console.log(
                "Warning! TransactionSlice was already set and will be overriden. This shouldn't be happening in production."
            );
        }
        this.transactionSlice = slice;
    }

    async getFrameworkAndSigner(chainId: number): Promise<[framework: Framework, signer: Signer]> {
        return await Promise.all([this.getFramework(chainId), this.getSigner(chainId)]);
    }
}

export const getConfig = SdkReduxConfig.getOrCreateSingleton;
export const getApiSlice = () => getConfig().getApiSlice();
export const getTransactionSlice = () => getConfig().getTransactionSlice();
export const getFramework = (chainId: number) => getConfig().getFramework(chainId);
export const getSigner = (chainId: number) => getConfig().getSigner(chainId);
export const getFrameworkAndSigner = (chainId: number) => getConfig().getFrameworkAndSigner(chainId);

const isEthersSigner = (value: any): value is Signer => !!value.getAddress;
