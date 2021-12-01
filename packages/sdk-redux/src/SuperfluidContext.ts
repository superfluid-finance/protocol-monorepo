import {Framework} from '@superfluid-finance/sdk-core';
import {Signer} from 'ethers';

const frameworks = new Map<number, () => Promise<Framework>>();
const signers = new Map<number, () => Promise<Signer>>();

/**
 * Context is a way for SDK-Redux reducers to access the current SDK-Core Framework instance
 * and the current active signer for transaction broadcasting.
 *
 * The context object is a way to do "dependency injection" or "service location", if you will.
 */
export interface SuperfluidContext {
    getFramework: (chainId: number) => Promise<Framework>;
    getSigner: (chainId: number) => Promise<Signer>;
    /**
     *
     * @param chainId
     * @param framework
     * @returns Returns itself for fluent API.
     */
    setFramework: (
        chainId: number,
        framework: (() => Promise<Framework>) | Framework
    ) => SuperfluidContext;
    /**
     *
     * @param chainId
     * @param signer
     * @returns Returns itself for fluent API.
     */
    setSigner: (
        chainId: number,
        signer: (() => Promise<Signer>) | Signer
    ) => SuperfluidContext;
    getFrameworkAndSigner: (
        chainId: number
    ) => Promise<[framework: Framework, signer: Signer]>;
}

/**
 * A preinitialized instance of a {@link SuperfluidContext}.
 */
export const preinitializedSuperfluidContext: SuperfluidContext = {
    getFramework: (chainId: number): Promise<Framework> => {
        const frameworkGetter = frameworks.get(chainId);
        if (!frameworkGetter)
            throw Error(
                `Don't know how to get Superfluid Framework. :( Please set up a *framework* source for chain [${chainId}].`
            );
        return frameworkGetter();
    },
    getSigner: (chainId: number): Promise<Signer> => {
        const signerGetter = signers.get(chainId);
        if (!signerGetter)
            throw Error(
                `Don't know how to get a signer. :( Please set up a *signer* source for chain [${chainId}].`
            );
        return signerGetter();
    },
    setFramework: (
        chainId: number,
        framework: (() => Promise<Framework>) | Framework
    ) => {
        if (framework instanceof Framework) {
            frameworks.set(chainId, () => Promise.resolve(framework));
        } else {
            frameworks.set(chainId, framework);
        }
        return preinitializedSuperfluidContext;
    },
    setSigner: (chainId: number, signer: (() => Promise<Signer>) | Signer) => {
        if (signer instanceof Signer) {
            signers.set(chainId, () => Promise.resolve(signer));
        } else {
            signers.set(chainId, signer);
        }
        return preinitializedSuperfluidContext;
    },
    getFrameworkAndSigner: async (
        chainId: number
    ): Promise<[framework: Framework, signer: Signer]> => {
        return await Promise.all([
            preinitializedSuperfluidContext.getFramework(chainId),
            preinitializedSuperfluidContext.getSigner(chainId),
        ]);
    },
};
