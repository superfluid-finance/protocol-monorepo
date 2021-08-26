import {getConfig, NetworkConfig} from '@superfluid-finance/js-sdk'
import { Framework } from '@superfluid-finance/js-sdk/src/Framework';
import _ from "lodash";

import {DAppSDK} from "./DAppSDK";
import { Network } from './Network';
import { mockAccountSource, setAccount } from './accountsSlice';
import { setNetwork  } from './networksSlice';
import { store } from './store';
import { SuperToken } from './superToken';

export const createDAppSDK = (superfluidSdk: Framework): DAppSDK => {
    return {
        reduxStore: store,
        superfluidSdk: superfluidSdk,
        subscribe(networkId: number, accountAddress: string): Promise<void> {
            const networkConfig = getConfig(networkId) as NetworkConfig;
            if (_.isEmpty(networkConfig)) return Promise.reject('Network not found.');

            const network = {
                id: networkId,
                superTokens: new Map<string, SuperToken>(),
            } as Network;

            const account = mockAccountSource.find(
                (x) => x.address === accountAddress && x.networkId === networkId
            );
            if (!account)
                return Promise.reject('Address from network not found.');

            this.reduxStore.dispatch(setAccount(account));
            this.reduxStore.dispatch(setNetwork(network));

            return Promise.resolve();
        },
    };
};
