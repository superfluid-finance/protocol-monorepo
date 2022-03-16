
import {getFramework} from '../../../sdkReduxConfig';
import {typeGuard} from '../../../utils';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {getMostSpecificStreamTag} from '../cacheTags/streamTags';
import {getMostSpecificTokenTag} from '../cacheTags/tokenTags';
import {ApiSliceEndpointBuilder} from '../sfApiSlice';

import {
    GetAllowanceForUpgradeToSuperToken,
    GetRealtimeBalance,
    GetRealtimeBalanceResult,
} from './queries';

export const addQueryEndpoints = (builder: ApiSliceEndpointBuilder) => ({
    getAllowanceForUpgradeToSuperToken: builder.query<string, GetAllowanceForUpgradeToSuperToken>({
        keepUnusedDataFor: 0, // We can't listen for "approval" event from Subgraph currently.
        providesTags: (_result, _error, arg) => [
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);

            const superToken = await framework.loadSuperToken(arg.superTokenAddress);

            const underlyingTokenAllowance = await superToken.underlyingToken.allowance({
                providerOrSigner: framework.settings.provider,
                owner: arg.accountAddress,
                spender: superToken.address,
            });

            return {
                data: underlyingTokenAllowance,
            };
        },
    }),
    getRealtimeBalance: builder.query<GetRealtimeBalanceResult, GetRealtimeBalance>({
        keepUnusedDataFor: 0, // We don't want to cache balance because it changes every second.
        providesTags: (_result, _error, arg) => [
            getMostSpecificIndexTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
                indexId: undefined,
            }),
            getMostSpecificStreamTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
            getMostSpecificTokenTag({
                chainId: arg.chainId,
                address1: arg.superTokenAddress,
                address2: arg.accountAddress,
                address3: undefined,
            }),
        ],
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const superToken = await framework.loadSuperToken(arg.superTokenAddress);
            const [realtimeBalance, netFlow] = await Promise.all([
                superToken.realtimeBalanceOf({
                    providerOrSigner: framework.settings.provider,
                    account: arg.accountAddress,
                    timestamp: arg.estimationTimestamp
                        ? arg.estimationTimestamp
                        : Math.floor(new Date().getTime() / 1000),
                }),
                superToken.getNetFlow({
                    account: arg.accountAddress,
                    providerOrSigner: framework.settings.provider,
                }),
            ]);
            return {
                data: typeGuard<GetRealtimeBalanceResult>({
                    availableBalanceWei: realtimeBalance.availableBalance.toString(),
                    depositWei: realtimeBalance.deposit.toString(),
                    owedDepositWei: realtimeBalance.owedDeposit.toString(),
                    timestamp: Math.floor(realtimeBalance.timestamp.getTime() / 1000),
                    netFlowRateWei: netFlow,
                }),
            };
        },
    }),
});
