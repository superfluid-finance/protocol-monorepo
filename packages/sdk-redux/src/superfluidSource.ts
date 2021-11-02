import { Framework } from '@superfluid-finance/sdk-core';
import {Signer} from "ethers";

const frameworks = new Map<number, Promise<Framework>>();
const signers = new Map<number, Promise<Signer>>();

export interface SuperfluidSource {
    getFramework: (chainId: number) => Promise<Framework>;
    setFramework: (chainId: number, frameworkPromise: Promise<Framework>) => void;
    getSigner: (chainId: number) => Promise<Signer>;
    setSigner: (
        chainId: number,
        frameworkPromise: Promise<Signer>
    ) => void;
    getFrameworkAndSigner: (chainId: number) => Promise<[framework: Framework, signer: Signer]>
}

export const superfluidSource: SuperfluidSource = {
    getFramework: (chainId: number): Promise<Framework> => {
        const frameworkPromise = frameworks.get(chainId);
        if (!frameworkPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *read* source for chain [${chainId}].`
            );
        return frameworkPromise;
    },
    getSigner: (chainId: number): Promise<Signer> => {
        const signerPromise = signers.get(chainId);
        if (!signerPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *write* source for chain [${chainId}].` // TODO(KK): error messages
            );
        return signerPromise;
    },
    setFramework: (chainId: number, frameworkPromise: Promise<Framework>) => {
        frameworks.set(chainId, frameworkPromise);
    },
    setSigner: (chainId: number, signerPromise: Promise<Signer>) => {
        signers.set(chainId, signerPromise);
    },
    getFrameworkAndSigner: async (chainId: number): Promise<[framework: Framework, signer: Signer]> => {
        return await Promise.all([
            superfluidSource.getFramework(chainId),
            superfluidSource.getSigner(chainId)
        ]);
    }
};
