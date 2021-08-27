import { Framework } from '@superfluid-finance/js-sdk/src/Framework';

import {DAppSDK} from "./DAppSDK";
import { store } from './store';

export const createDAppSDK = (superfluidSdk: Framework): DAppSDK => {
    return {
        reduxStore: store,
        superfluidSdk: superfluidSdk,
    };
};
