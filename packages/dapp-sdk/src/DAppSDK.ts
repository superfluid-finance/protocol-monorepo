import {Framework} from "@superfluid-finance/js-sdk/src/Framework";

import { StoreType } from './store';

export interface DAppSDK {
    reduxStore: StoreType;
    subscribe(networkId: number, accountAddress: string): Promise<void>;
    superfluidSdk: Framework
}

// export interface Operations {
//     stream: StreamOperations;
// }
//
// export interface StreamOperations {
//     createStream(); // Promise? Transaction?
// }
