import { Framework } from '@superfluid-finance/sdk-core';
import { Signer } from 'ethers';

const frameworks = new Map<number, () => Promise<Framework>>();
const signers = new Map<number, () => Promise<Signer>>();

export const superfluidContext = {
    getFramework: (chainId: number): Promise<Framework> => {
        const frameworkGetter = frameworks.get(chainId);
        if (!frameworkGetter)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *read* source for chain [${chainId}].`
            );
        return frameworkGetter();
    },
    getSigner: (chainId: number): Promise<Signer> => {
        const signerGetter = signers.get(chainId);
        if (!signerGetter)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *write* source for chain [${chainId}].` // TODO(KK): error messages
            );
        return signerGetter();
    },
    setFramework: (chainId: number, framework: (() => Promise<Framework>) | Framework) => {
        if (framework instanceof Framework) {
            frameworks.set(chainId, () => Promise.resolve(framework));
        } else {
            frameworks.set(chainId, framework);
        }
        return superfluidContext;
    },
    setSigner: (chainId: number, signer: (() => Promise<Signer>) | Signer) => {
        if (signer instanceof Signer) {
            signers.set(chainId, () => Promise.resolve(signer));
        } else {
            signers.set(chainId, signer);
        }
        return superfluidContext;
    },
    getFrameworkAndSigner: async (
        chainId: number
    ): Promise<[framework: Framework, signer: Signer]> => {
        return await Promise.all([
            superfluidContext.getFramework(chainId),
            superfluidContext.getSigner(chainId),
        ]);
    },
};

export type SuperfluidContext = typeof superfluidContext;
