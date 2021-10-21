import {configureStore, Dispatch} from "@reduxjs/toolkit";
import { createSuperfluidSlice } from "dapp-sdk";
import {TypedUseSelectorHook, useDispatch, useSelector} from "react-redux";

const [
    superfluidFrameworkSource,
    superfluidApiSlice,
    superfluidTransactionSlice,
] = createSuperfluidSlice();

export const store = configureStore({
    reducer: {
        [superfluidApiSlice.reducerPath]: superfluidApiSlice.reducer,
        "transactions": superfluidTransactionSlice.reducer,
    },
    middleware: (getDefaultMiddleware) =>
        getDefaultMiddleware().concat(superfluidApiSlice.middleware),
});

export type AppDispatch = typeof store.dispatch;
export type RootState = ReturnType<typeof store.getState>;
export { superfluidFrameworkSource };

// eslint-disable-next-line @typescript-eslint/explicit-module-boundary-types
export const useAppDispatch = () => useDispatch<Dispatch>();
export const useAppSelector: TypedUseSelectorHook<RootState> = useSelector;
