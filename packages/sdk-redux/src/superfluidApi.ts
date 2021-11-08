import { rtkQuerySlice } from './redux-slices/rtk-query/rtkQuerySlice';
import {
    SuperfluidReduxTransactionSliceType,
    transactionSlice,
} from './redux-slices/transactions/transactionSlice';
import {
    superfluidSource as superfluidFrameworkSourcePreinitialized,
    SuperfluidSource,
} from './superfluidSource';

export let initializedSuperfluidSource: SuperfluidSource = null!;

export type SuperfluidReduxApiSliceType = typeof rtkQuerySlice;

export const createPieces = (
    frameworkSource?: SuperfluidSource
): [
    superfluidFrameworkSource: SuperfluidSource,
    superfluidApiSlice: SuperfluidReduxApiSliceType,
    superfluidTransactionSlice: SuperfluidReduxTransactionSliceType
] => {
    if (initializedSuperfluidSource) {
        throw Error("You shouldn't create the slice multiple times.");
    }

    if (frameworkSource) {
        initializedSuperfluidSource = frameworkSource;
    } else {
        initializedSuperfluidSource = superfluidFrameworkSourcePreinitialized;
    }

    return [initializedSuperfluidSource, rtkQuerySlice, transactionSlice];
};
