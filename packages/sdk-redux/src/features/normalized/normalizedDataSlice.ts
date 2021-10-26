import {createAsyncThunk, createSlice, Draft} from '@reduxjs/toolkit';
import _ from "lodash";

import {DAppSdkRootState} from "../../store";

import {Account, AccountCompositeKey, StreamIndex} from "./Account";
import {Network, StreamCompositeKey, StreamDetails} from './Network';
import {asyncMockDataSource} from "./asyncDataSource";

export interface NormalizedDataState {
    isLoading: boolean,
    error: string | null,
    networks: { [id: number]: Network },
}

export const fetchAccount = createAsyncThunk<Account,
    AccountCompositeKey,
    { state: DAppSdkRootState; rejectValue: string }>(
    'fetchAccount',
    async (arg: AccountCompositeKey, thunkApi) => {
        const state = thunkApi.getState().normalizedData;

        if (!_.has(state.networks, arg.chainId))
            return thunkApi.rejectWithValue('Network not found.');

        const account = await asyncMockDataSource.fetchAccount(arg);

        if (!account)
            return thunkApi.rejectWithValue('Address not found.');

        return account;
    }
);

interface FetchAccountStreamIndexesArgs extends AccountCompositeKey {
    isActive: boolean
}

export const fetchAccountStreamIndexes = createAsyncThunk<StreamIndex[], FetchAccountStreamIndexesArgs, { state: DAppSdkRootState; rejectValue: string }>(
    'fetchAccountStreamIndexes',
    async (arg: FetchAccountStreamIndexesArgs, thunkApi) => {
        const {chainId, accountAddress, isActive} = arg;
        const state = thunkApi.getState().normalizedData;

        if (!_.has(state.networks, chainId)) {
            return thunkApi.rejectWithValue('Network not found.');
        }

        if (!_.has(state.networks[chainId].accounts, accountAddress)) {
            return thunkApi.rejectWithValue('Account not found.');
        }

        return await asyncMockDataSource.fetchAccountStreamIndexes(
            chainId,
            accountAddress,
            isActive
        );
    })

export const fetchStreamDetails = createAsyncThunk<StreamDetails, StreamCompositeKey, { state: DAppSdkRootState; rejectValue: string }>(
    'fetchStreamDetails',
    async (arg: StreamCompositeKey, thunkApi) => {
        const state = thunkApi.getState().normalizedData;

        if (!_.has(state.networks, arg.networkId)) {
            return thunkApi.rejectWithValue('Network not found.');
        }

        const streamDetails = await asyncMockDataSource.fetchStreamDetails(arg);
        if (!streamDetails)
            return thunkApi.rejectWithValue('Stream not found.');

        return streamDetails;
    })

const initialState: NormalizedDataState = {
    isLoading: false,
    error: null,
    networks: {
        //
        // ETHEREUM
        //
        31337: {
            id: 31337,
            // for local testing hardhat
            nativeTokenSymbol: "ETH",
            streamDetails: {},
            accounts: {}
        },
        1337: {
            id: 1337,
            // for local testing localhost
            nativeTokenSymbol: "ETH",
            streamDetails: {},
            accounts: {}
        },
        4447: {
            id: 4447,
            // for local testing (truffle internal ganache)
            nativeTokenSymbol: "ETH",
            streamDetails: {},
            accounts: {}
        },
        5777: {
            id: 5777,
            // for local testing (external ganache)
            nativeTokenSymbol: "ETH",
            streamDetails: {},
            accounts: {}
        },
        5: {
            id: 5,
            // goerli
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E",
            streamDetails: {},
            accounts: {}
        },
        4: {
            id: 4,
            // rinkeby
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A",
            streamDetails: {},
            accounts: {}
        },
        3: {
            id: 3,
            // ropsten
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
            streamDetails: {},
            accounts: {}
        },
        42: {
            id: 42,
            // kovan
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B",
            streamDetails: {},
            accounts: {}
        },

        //
        // MATIC: https://docs.matic.network/docs/develop/network-details/network/
        //
        137: {
            id: 137,
            // (matic) mainnet
            nativeTokenSymbol: "MATIC",
            resolverAddress: "0xE0cc76334405EE8b39213E620587d815967af39C",
            streamDetails: {},
            accounts: {}
        },

        80001: {
            id: 80001,
            // (matic) mumbai testnet
            nativeTokenSymbol: "MATIC",
            resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3",
            streamDetails: {},
            accounts: {}
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        0x64: {
            id: 0x64,
            nativeTokenSymbol: "xDAI",
            resolverAddress: "0xD2009765189164b495c110D61e4D301729079911",
            streamDetails: {},
            accounts: {}
        },

        // ARTIS
        0x03c401: {
            id: 0x03c401,
            // (artis) tau1 testnet
            resolverAddress: "0x79D426CD219eDCFEB2dCbcf7ea0F8B3642C56F47",
            streamDetails: {},
            accounts: {}
        },

        //
        // ARBITRUM (testnet rinkeby)
        //
        421611: {
            id: 421611,
            // arbitrum testnet
            resolverAddress: "0x79D426CD219eDCFEB2dCbcf7ea0F8B3642C56F47",
            streamDetails: {},
            accounts: {}
        }
    },
};

export const normalizedDataSlice = createSlice({
    name: 'normalized',
    initialState,
    reducers: {},
    extraReducers: (builder) => {
        builder
            // #Loading
            .addCase(fetchAccount.pending, (state: Draft<NormalizedDataState>) => {
                state.error = null;
                state.isLoading = true;
            })
            .addCase(fetchAccountStreamIndexes.pending, (state: Draft<NormalizedDataState>) => {
                state.error = null;
                state.isLoading = true;
            })
            .addCase(fetchStreamDetails.pending, (state: Draft<NormalizedDataState>) => {
                state.error = null;
                state.isLoading = true;
            })
            // end #Loading

            // #Error
            .addCase(fetchAccount.rejected, (state: Draft<NormalizedDataState>, action) => {
                state.isLoading = false;
                state.error = action.payload || "Error.";
            })
            .addCase(fetchAccountStreamIndexes.rejected, (state: Draft<NormalizedDataState>, action) => {
                state.isLoading = false;
                state.error = action.payload || "Error.";
            })
            .addCase(fetchStreamDetails.rejected, (state: Draft<NormalizedDataState>, action) => {
                state.isLoading = false;
                state.error = action.payload || "Error.";
            })
            // end #Error

            // #Fulfilled
            .addCase(fetchAccount.fulfilled, (state: Draft<NormalizedDataState>, action) => {
                const {chainId, accountAddress} = action.meta.arg;

                const previous = state.networks[chainId].accounts[accountAddress];
                state.networks[chainId].accounts[accountAddress] = {...previous, ...action.payload};

                state.isLoading = false;
            })
            .addCase(fetchAccountStreamIndexes.fulfilled, (state: Draft<NormalizedDataState>, action) => {
                const {chainId, accountAddress, isActive} = action.meta.arg;

                const account = state.networks[chainId].accounts[accountAddress];
                if (isActive) {
                    account.activeStreams = action.payload;
                } else {
                    account.inactiveStreams = action.payload;
                }

                state.isLoading = false;
            })
            .addCase(fetchStreamDetails.fulfilled, (state: Draft<NormalizedDataState>, action) => {
                const {networkId, transactionHash} = action.meta.arg;

                const previous = state.networks[networkId].streamDetails[transactionHash];
                state.networks[networkId].streamDetails[transactionHash] = {...previous, ...action.payload};

                state.isLoading = false;
            })
            // end #Fulfilled
    }
});
