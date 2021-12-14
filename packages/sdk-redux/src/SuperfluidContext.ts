import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

import {SfApiSliceInferredType} from './redux-slices/rtk-query/sfApiSlice';
import {SfTransactionSliceType} from './redux-slices/transactions/createTransactionSlice';

interface FrameworkAndSignerLocator extends FrameworkLocator, SignerLocator {
    getFrameworkAndSigner: (chainId: number) => Promise<[framework: Framework, signer: Signer]>;
}

interface FrameworkLocator {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (
        chainId: number,
        framework: (() => Promise<Framework>) | Framework
    ) => FrameworkLocator & SignerLocator;
}

interface SignerLocator {
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (chainId: number, signer: (() => Promise<Signer>) | Signer) => FrameworkLocator & SignerLocator;
}

interface ApiSliceLocator {
    getApiSlice: () => SfApiSliceInferredType;
    setApiSlice: (slice: SfApiSliceInferredType) => void;
}

interface TransactionSliceLocator {
    getTransactionSlice: () => SfTransactionSliceType;
    setTransactionSlice: (slice: SfTransactionSliceType) => void;
}

export default class SuperfluidContext implements FrameworkAndSignerLocator, ApiSliceLocator, TransactionSliceLocator {
    apiSlice: SfApiSliceInferredType | undefined;
    transactionSlice: SfTransactionSliceType | undefined;
    frameworks = new Map<number, () => Promise<Framework>>();
    signers = new Map<number, () => Promise<Signer>>();

    static getOrCreateSingleton(): SuperfluidContext {
        if (!globalThis.superfluidContext) {
            globalThis.superfluidContext = new SuperfluidContext();
        }
        return globalThis.superfluidContext;
    }

    getApiSlice(): SfApiSliceInferredType {
        if (!this.apiSlice) {
            throw Error('The ApiSlice has not been set. Are you sure you initialized SDK-Redux properly?');
        }

        return this.apiSlice;
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

    setFramework(chainId: number, framework: (() => Promise<Framework>) | Framework): FrameworkLocator & SignerLocator {
        if (isFramework(framework)) {
            this.frameworks.set(chainId, () => Promise.resolve(framework));
        } else {
            this.frameworks.set(chainId, framework);
        }
        return this;
    }

    setSigner(chainId: number, signer: (() => Promise<Signer>) | Signer): FrameworkLocator & SignerLocator {
        if (isSigner(signer)) {
            this.signers.set(chainId, () => Promise.resolve(signer));
        } else {
            this.signers.set(chainId, signer);
        }
        return this;
    }

    setTransactionSlice(slice: SfTransactionSliceType): void {
        if (this.transactionSlice) {
            console.log(
                "Warning! TransactionSlice was already set and will be osverriden. This shouldn't be happening in production."
            );
        }
        this.transactionSlice = slice;
    }

    async getFrameworkAndSigner(chainId: number): Promise<[framework: Framework, signer: Signer]> {
        return await Promise.all([this.getFramework(chainId), this.getSigner(chainId)]);
    }
}

export const getSuperfluidContext = SuperfluidContext.getOrCreateSingleton;
export const getApiSlice = () => getSuperfluidContext().getApiSlice();
export const getTransactionSlice = () => getSuperfluidContext().getTransactionSlice();
export const getFramework = (chainId: number) => getSuperfluidContext().getFramework(chainId);
export const getSigner = (chainId: number) => getSuperfluidContext().getSigner(chainId);
export const getFrameworkAndSigner = (chainId: number) => getSuperfluidContext().getFrameworkAndSigner(chainId);

// For some reason "framework instanceof Framework" didn't work.
const isFramework = (value: any): value is Framework => {
    return !!value.cfaV1;
};

// For some reason "signer instanceof Signer" didn't work.
const isSigner = (value: any): value is Signer => {
    return !!value.getAddress;
};
