import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type GetRealtimeBalanceArg = QueryArg & {
    superTokenAddress: string;
    accountAddress: string;
    timestamp?: string;
}

export type GetRealtimeBalanceResult = {
    availableBalance: string;
    netFlowRate: string;
    deposit: string;
    owedDeposit: string;
    timestamp: string;
}

// TODO(KK): Clean up the timestamp flooring here...
// TODO(KK): Consider keeping netflow separate after all...

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getRealtimeBalance: builder.query<
            GetRealtimeBalanceResult,
            GetRealtimeBalanceArg
        >({
            keepUnusedDataFor: 0, // We don't want to cache balance because it changes every second.
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );
                const returnData: GetRealtimeBalanceResult = await Promise.all([
                    superToken.realtimeBalanceOf({
                        providerOrSigner: framework.settings.provider,
                        account: arg.accountAddress,
                        timestamp: arg.timestamp
                            ? arg.timestamp
                            : Math.floor(new Date().getTime() / 1000).toString()
                    }),
                    superToken.getNetFlow({
                        account: arg.accountAddress,
                        providerOrSigner: framework.settings.provider,
                    }),
                ]).then((x) => ({
                    availableBalance: x[0].availableBalance.toString(),
                    deposit: x[0].deposit.toString(),
                    owedDeposit: x[0].owedDeposit.toString(),
                    timestamp: Math.floor(x[0].timestamp.getTime() / 1000).toString(),
                    netFlowRate: x[1],
                }));
                return {
                    data: returnData,
                };
            },
        }),
    }),
    overrideExisting: false,
});

export const { useGetRealtimeBalanceQuery, useLazyGetRealtimeBalanceQuery } =
    extendedApi;
