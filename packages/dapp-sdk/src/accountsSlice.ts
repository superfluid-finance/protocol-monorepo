import { createSlice, Draft, PayloadAction } from '@reduxjs/toolkit';

import { Account } from './Account';

export interface AccountsState {
    accounts: { [key: string]: Account };
}

const initialState: AccountsState = {
    accounts: {}
};

export const accountsSlice = createSlice({
    name: 'accounts',
    initialState,
    reducers: {
        setAccount: (state: Draft<AccountsState>, action: PayloadAction<Account>) => {
            state.accounts[action.payload.address] = action.payload;
        }
    }
});

export const { setAccount } = accountsSlice.actions;

export default accountsSlice.reducer;

export const mockAccountSource = [
    {
        address: '0x5188a513fF9E71C6D958800c4722978B95fe5a14',
        networkId: '100'
    },
    {
        address: '0x605b242549e48b9Fa5800a36663405b019B0B433',
        networkId: '200'
    }
];
