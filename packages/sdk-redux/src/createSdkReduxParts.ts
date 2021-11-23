import {
    rtkQuerySlice,
    SuperfluidApiReduxSliceType,
} from './redux-slices/rtk-query/rtkQuerySlice';
import {
    SuperfluidTransactionReduxSlice,
    transactionSlice,
} from './redux-slices/transactions/transactionSlice';
import {
    superfluidContext as contextPreinitialized,
    SuperfluidContext,
} from './superfluidContext';

export let initializedContext: SuperfluidContext = null!;

export const createSdkReduxParts = (
    context?: SuperfluidContext
): {
    context: SuperfluidContext;
    apiSlice: SuperfluidApiReduxSliceType;
    transactionSlice: SuperfluidTransactionReduxSlice;
} => {
    if (initializedContext) {
        throw Error("You shouldn't create the slice multiple times.");
    }

    if (context) {
        initializedContext = context;
    } else {
        initializedContext = contextPreinitialized;
    }

    return {
        context: initializedContext,
        apiSlice: rtkQuerySlice,
        transactionSlice: transactionSlice,
    };
};
