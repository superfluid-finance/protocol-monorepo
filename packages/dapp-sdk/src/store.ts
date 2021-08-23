import { configureStore } from '@reduxjs/toolkit';

import addressReducer from './accountsSlice';
import chainReducer from './chainsSlice';

export const store = configureStore({
    reducer: {
        chains: chainReducer,
        addresses: addressReducer
    }
});

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>
// Inferred type: {posts: PostsState, comments: CommentsState, users: UsersState}
export type AppDispatch = typeof store.dispatch
