import { Framework } from '@superfluid-finance/js-sdk';
import { NetworkName } from '@superfluid-finance/sdk-core';

const frameworkForReading = new Map<NetworkName, Promise<Framework>>();
const frameworkForWriting = new Map<NetworkName, Promise<Framework>>();

export interface SuperfluidFrameworkSource {
    getForRead: (networkName: NetworkName) => Promise<Framework>;
    getForWrite: (networkName: NetworkName) => Promise<Framework>;
    setForRead: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => void;
    setForWrite: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => void;
    setForReadAndWrite: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => void;
}

export const superfluidFrameworkSource: SuperfluidFrameworkSource = {
    getForRead: (networkName: NetworkName): Promise<Framework> => {
        const frameworkPromise = frameworkForReading.get(networkName);
        if (!frameworkPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *read* source for network [${networkName}].`
            );
        return frameworkPromise;
    },
    getForWrite: (networkName: NetworkName): Promise<Framework> => {
        const frameworkPromise = frameworkForWriting.get(networkName);
        if (!frameworkPromise)
            throw Error(
                `Don't know how to get Superfluid. :( Please set up a *write* source for network [${networkName}].`
            );
        return frameworkPromise;
    },
    setForRead: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => {
        console.log({
            method: 'setForRead',
            args: {
                networkName,
                frameworkPromise,
            },
        });
        frameworkForReading.set(networkName, frameworkPromise);
    },
    setForWrite: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => {
        console.log({
            method: 'setForWrite',
            args: {
                networkName,
                frameworkPromise,
            },
        });
        frameworkForWriting.set(networkName, frameworkPromise);
    },
    setForReadAndWrite: (
        networkName: NetworkName,
        frameworkPromise: Promise<Framework>
    ) => {
        superfluidFrameworkSource.setForRead(networkName, frameworkPromise);
        superfluidFrameworkSource.setForWrite(networkName, frameworkPromise);
    },
};
