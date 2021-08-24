import { createSlice, Draft, PayloadAction } from '@reduxjs/toolkit';

import { Network } from './Network';
import { SuperToken } from './superToken';

export interface NetworksState {
    networks: { [key: string]: Network };
}

const initialState: NetworksState = {
    networks: {}
};

export const networksSlice = createSlice({
    name: 'networks',
    initialState,
    reducers: {
        setNetwork: (state: Draft<NetworksState>, action: PayloadAction<Network>) => {
            state.networks[action.payload.id] = action.payload;
        }
    }
});

export const { setNetwork } = networksSlice.actions;

export default networksSlice.reducer;

export const mockNetworkSource = [
    {
        id: '100',
        superTokens: new Map<string, SuperToken>([
            ['superDAI', {
                address: '0x3814B4dE666735C12245395767bDa5AC27e29616',
                symbol: 'superDAI'
            }],
            ['superETH', {
                address: '0x8166473aE41867ECb9D29b72B91BD0bE5Cb639fe',
                symbol: 'superETH'
            }]
        ])
    },
    {
        id: '200',
        superTokens: new Map<string, SuperToken>([
            ['superDAI', {
                address: '0x70020b7C19dA4912A5856F208763522E063D7264',
                symbol: 'superDAI'
            }],
            ['superETH', {
                address: '0x21374A5092AA7Ff694a79734bC056d6e5281c612',
                symbol: 'superETH'
            }]
        ])
    }
];
