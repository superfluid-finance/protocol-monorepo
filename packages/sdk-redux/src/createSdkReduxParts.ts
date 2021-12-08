import {
    preinitializedSuperfluidContext,
    SuperfluidContext,
} from './SuperfluidContext';
import {
    rtkQuerySlice,
    SuperfluidApiReduxSliceType,
} from './redux-slices/rtk-query/rtkQuerySlice';
import {
    SuperfluidTransactionReduxSlice,
    transactionSlice,
} from './redux-slices/transactions/transactionSlice';

export const getSfContext: () => SuperfluidContext = () =>
    globalThis.superfluidContext;

/**
 * First initialization point of SDK-Redux.
 * Creates the necessary parts to set up redux store and dependency injection for SDk-Redux.
 * @param superfluidContext Optional user-defined instance of {@link SuperfluidContext}.
 */
export const createSdkReduxParts = (
    superfluidContext?: SuperfluidContext
): {
    /**
     * Returns the initialized instance of {@link SuperfluidContext}. When user passes in their own instance then that is returned.
     */
    superfluidContext: SuperfluidContext;
    /**
     * Instance of {@link SuperfluidApiReduxSliceType} to plug into the redux store.
     */
    apiSlice: SuperfluidApiReduxSliceType;
    /**
     * Instance of {@link SuperfluidTransactionReduxSlice} to plug into the Redux store.
     */
    transactionSlice: SuperfluidTransactionReduxSlice;
} => {
    if (superfluidContext) {
        globalThis.superfluidContext = superfluidContext;
    } else {
        globalThis.superfluidContext = preinitializedSuperfluidContext;
    }

    return {
        superfluidContext: getSfContext(),
        apiSlice: rtkQuerySlice,
        transactionSlice: transactionSlice,
    };
};
