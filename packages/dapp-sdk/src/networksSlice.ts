import { createSlice, Draft, PayloadAction } from '@reduxjs/toolkit';

import { Network } from './Network';

export interface NetworksState {
    networks: { [key: number]: Network };
}

const initialState: NetworksState = {
    networks: {},
};

export const networksSlice = createSlice({
    name: 'networks',
    initialState,
    reducers: {
        setNetwork: (
            state: Draft<NetworksState>,
            action: PayloadAction<Network>
        ) => {
            state.networks[action.payload.id] = action.payload;
        },
    },
});

export const { setNetwork } = networksSlice.actions;

export default networksSlice.reducer;
