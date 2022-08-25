import { configureStore, Dispatch } from "@reduxjs/toolkit";
import {
    createApiWithReactHooks,
    initializeSfApiSlice,
    initializeSfTransactionSlice,
    setFrameworkForSdkRedux,
} from "@superfluid-finance/sdk-redux";
import { TypedUseSelectorHook, useDispatch, useSelector } from "react-redux";
import { Framework } from "@superfluid-finance/sdk-core";
import { ethers } from "ethers";
import { createWrapper, HYDRATE } from "next-redux-wrapper";

export const { sfApi } = initializeSfApiSlice((options) =>
    createApiWithReactHooks({
        ...options,
        extractRehydrationInfo(action, { reducerPath }) {
            if (action.type === HYDRATE) {
                return action.payload[reducerPath];
            }
        },
    })
);
export const { sfTransactions } = initializeSfTransactionSlice();

export const makeStore = () => {
    const chainId = 5;

    setFrameworkForSdkRedux(chainId, () =>
        Framework.create({
            chainId,
            provider: new ethers.providers.InfuraProvider(
                chainId,
                process.env.NEXT_PUBLIC_INFURA_ID
            ),
        })
    );

    return configureStore({
        reducer: {
            sfApi: sfApi.reducer,
            sfTransactions: sfTransactions.reducer,
        },
        middleware: (getDefaultMiddleware) =>
            getDefaultMiddleware().concat(sfApi.middleware),
    });
};

export type AppStore = ReturnType<typeof makeStore>;
export type RootState = ReturnType<AppStore["getState"]>;
export type AppDispatch = AppStore["dispatch"];

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useAppDispatch = () => useDispatch<Dispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;

// NOTE: The serialization is important to override because RTK-Query will have some "undefined" values in the state which Next.js doesn't like to serialize by default.
export const wrapper = createWrapper<AppStore>(makeStore, {
    debug: true,
    serializeState: (state) => JSON.stringify(state),
    deserializeState: (state) => JSON.parse(state),
});
