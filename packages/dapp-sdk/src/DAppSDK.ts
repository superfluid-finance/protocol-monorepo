import {Framework} from "@superfluid-finance/js-sdk/src/Framework";

import { DAppSdkStoreType } from './store';

export interface DAppSDK {
    reduxStore: DAppSdkStoreType;
    superfluidSdk: Framework
}
