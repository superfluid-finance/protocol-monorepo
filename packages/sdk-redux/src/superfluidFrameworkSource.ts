import { Framework } from '@superfluid-finance/js-sdk';

const frameworkForReading = new Map<number, Promise<Framework>>();
const frameworkForWriting = new Map<number, Promise<Framework>>();

export interface SuperfluidFrameworkSource {
    getForRead: (chainId: number) => Promise<Framework>;
    getForWrite: (chainId: number) => Promise<Framework>;
    setForRead: (chainId: number, frameworkPromise: Promise<Framework>) => void;
    setForWrite: (
        chainId: number,
        frameworkPromise: Promise<Framework>
    ) => void;
    setForReadAndWrite: (
        chainId: number,
        frameworkPromise: Promise<Framework>
    ) => void;
}

export const superfluidFrameworkSource: SuperfluidFrameworkSource = {
    getForRead: (chainId: number): Promise<Framework> => {
        const frameworkPromise = frameworkForReading.get(chainId);
        if (!frameworkPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *read* source for chain [${chainId}].`
            );
        return frameworkPromise;
    },
    getForWrite: (chainId: number): Promise<Framework> => {
        const frameworkPromise = frameworkForWriting.get(chainId);
        if (!frameworkPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *write* source for chain [${chainId}].`
            );
        return frameworkPromise;
    },
    setForRead: (chainId: number, frameworkPromise: Promise<Framework>) => {
        frameworkForReading.set(chainId, frameworkPromise);
    },
    setForWrite: (chainId: number, frameworkPromise: Promise<Framework>) => {
        frameworkForWriting.set(chainId, frameworkPromise);
    },
    setForReadAndWrite: (
        chainId: number,
        frameworkPromise: Promise<Framework>
    ) => {
        superfluidFrameworkSource.setForRead(chainId, frameworkPromise);
        superfluidFrameworkSource.setForWrite(chainId, frameworkPromise);
    },
};
