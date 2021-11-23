import { Framework } from '@superfluid-finance/sdk-core';
import { Signer } from 'ethers';

const frameworks = new Map<number, () => Promise<Framework>>();
const signers = new Map<number, () => Promise<Signer>>();

export const superfluidSource = {
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
    setFramework: (chainId: number, getFramework: () => Promise<Framework>) => {
        frameworks.set(chainId, getFramework);
    },
    setSigner: (chainId: number, getSigner: () => Promise<Signer>) => {
        signers.set(chainId, getSigner);
    },
    getFrameworkAndSigner: async (
        chainId: number
    ): Promise<[framework: Framework, signer: Signer]> => {
        return await Promise.all([
            superfluidSource.getFramework(chainId),
            superfluidSource.getSigner(chainId),
        ]);
    },
};

export type SuperfluidSource = typeof superfluidSource;
