import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';

export type GetRealtimeBalanceArg = QueryArg & {
    superTokenAddress: string;
    accountAddress: string;
    estimationTimestamp?: number;
};

export type GetRealtimeBalanceResult = {
    availableBalanceWei: string;
    netFlowRateWei: string;
    depositWei: string;
    owedDepositWei: string;
    timestamp: number;
};

// TODO(KK): Clean up the timestamp flooring here...
// TODO(KK): Consider keeping netflow separate after all...

export const { useGetRealtimeBalanceQuery, useLazyGetRealtimeBalanceQuery } =
    rtkQuerySlice.injectEndpoints({
        endpoints: (builder) => ({
            getRealtimeBalance: builder.query<
                GetRealtimeBalanceResult,
                GetRealtimeBalanceArg
            >({
                keepUnusedDataFor: 0, // We don't want to cache balance because it changes every second.
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );
                    const superToken = await framework.loadSuperToken(
                        arg.superTokenAddress
                    );
                    const returnData: GetRealtimeBalanceResult =
                        await Promise.all([
                            superToken.realtimeBalanceOf({
                                providerOrSigner: framework.settings.provider,
                                account: arg.accountAddress,
                                timestamp: arg.estimationTimestamp
                                    ? arg.estimationTimestamp
                                    : Math.floor(
                                          new Date().getTime() / 1000
                                      ),
                            }),
                            superToken.getNetFlow({
                                account: arg.accountAddress,
                                providerOrSigner: framework.settings.provider,
                            }),
                        ]).then((x) => ({
                            availableBalanceWei:
                                x[0].availableBalance.toString(),
                            depositWei: x[0].deposit.toString(),
                            owedDepositWei: x[0].owedDeposit.toString(),
                            timestamp: Math.floor(
                                x[0].timestamp.getTime() / 1000
                            ),
                            netFlowRateWei: x[1],
                        }));
                    return {
                        data: returnData,
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
