import {Framework} from "@superfluid-finance/js-sdk/src/Framework";

import { StoreType } from './store';

export interface DAppSDK {
    reduxStore: StoreType;
    superfluidSdk: Framework
}
