import { createSlice, Draft, PayloadAction } from '@reduxjs/toolkit';

import { Chain } from './chain';

export interface ChainState {
    chains: Map<string, Chain>;
}

const initialState: ChainState = {
    chains: new Map<string, Chain>()
};

export const chainsSlice = createSlice({
    name: 'chain',
    initialState,
    reducers: {
        setChain: (state: Draft<ChainState>, action: PayloadAction<Chain>) => {
            state.chains.set(action.payload.id, action.payload);
        }
    }
});

export const { setChain } = chainsSlice.actions;

export default chainsSlice.reducer;
