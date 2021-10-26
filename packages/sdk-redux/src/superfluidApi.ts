import { rtkQuerySlice } from './redux-slices/rtk-query/rtkQuerySlice';
import {
    SuperfluidReduxTransactionSliceType,
    transactionSlice,
} from './redux-slices/transactions/transactionSlice';
import {
    SuperfluidFrameworkSource,
    superfluidFrameworkSource as superfluidFrameworkSourcePreinitialized,
} from './superfluidFrameworkSource';

export let initializedSuperfluidFrameworkSource: SuperfluidFrameworkSource =
    null!;

export interface FlowDetails {
    timestamp: number;
    flowRate: string;
    deposit: string;
    owedDeposit: string;
}

export interface Flow {
    sender: string;
    receiver: string;
    flowRate: string;
    superToken: string;
}

export type SuperfluidReduxApiSliceType = typeof rtkQuerySlice;

export const createSuperfluidSlice = (
    frameworkSource?: SuperfluidFrameworkSource
): [
    superfluidFrameworkSource: SuperfluidFrameworkSource,
    superfluidApiSlice: SuperfluidReduxApiSliceType,
    superfluidTransactionSlice: SuperfluidReduxTransactionSliceType
] => {
    if (initializedSuperfluidFrameworkSource) {
        throw Error("You shouldn't create the slice multiple times.");
    }

    if (frameworkSource) {
        initializedSuperfluidFrameworkSource = frameworkSource;
    } else {
        initializedSuperfluidFrameworkSource =
            superfluidFrameworkSourcePreinitialized;
    }

    return [
        initializedSuperfluidFrameworkSource,
        rtkQuerySlice,
        transactionSlice,
    ];
};
