import { configureStore } from '@reduxjs/toolkit';

import accountsReducer from './accountsSlice';
import networksReducer from './networksSlice';

export const store = configureStore({
    reducer: {
        networks: networksReducer,
        accounts: accountsReducer
    }
});

// Infer the `RootState` and `AppDispatch` types from the store itself
export type RootState = ReturnType<typeof store.getState>
// Inferred type: {posts: PostsState, comments: CommentsState, users: UsersState}
export type AppDispatch = typeof store.dispatch
export type StoreType = typeof store;
