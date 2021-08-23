import { createSlice, Draft, PayloadAction } from '@reduxjs/toolkit';

import { Account } from './account';

// TODO: Use address map?

export interface AccountsState {
    accounts: Map<string, Account>;
}

const initialState: AccountsState = {
    accounts: new Map<string, Account>()
};

export const accountsSlice = createSlice({
    name: 'address',
    initialState,
    reducers: {
        setAccount: (state : Draft<AccountsState>, action: PayloadAction<Account>) => {
            state.accounts.set(action.payload.address, action.payload);
        }
    }
});

export const { setAccount } = accountsSlice.actions;

export default accountsSlice.reducer;
