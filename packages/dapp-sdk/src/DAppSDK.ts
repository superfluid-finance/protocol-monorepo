// import { Framework } from '@superfluid-finance/js-sdk/src/Framework';

import { Account } from './Account';
import { mockAccountSource, setAccount } from './accountsSlice';
import { mockNetworkSource, setNetwork } from './networksSlice';
import { store, StoreType } from './store';

export interface DAppSDK {
    reduxStore : StoreType,
    subscribe(networkId: string, accountAddress: string): Promise<void>;
    scope(account: Account): void;
}

export const createDAppSDK = () => {
    return {
        reduxStore: store,
        subscribe(networkId: string, accountAddress: string): Promise<void> {
            const network = mockNetworkSource.find(x => x.id === networkId);
            if (!network)
                return Promise.reject("Network not found.");

            const account = mockAccountSource.find(x => x.address === accountAddress && x.networkId === networkId);
            if (!account)
                return Promise.reject("Address from network not found.");

            this.reduxStore.dispatch(setAccount(account));
            this.reduxStore.dispatch(setNetwork(network));

            return Promise.resolve();
        },
        scope() {

        }
    }
}
