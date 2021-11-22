import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingNumber, QueryArg} from '../../baseArg';
import {
    getMostSpecificIndexTag,
    getMostSpecificStreamTag, getMostSpecificTokenTag,
    rtkQuerySlice,
} from '../rtkQuerySlice';
import { typeGuard } from '../../../utils';

export type GetRealtimeBalanceArg = QueryArg & {
    superTokenAddress: string;
    accountAddress: string;
    estimationTimestamp: number | NothingNumber;
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
                    })
                ],
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
                            arg.chainId
                        );
                    const superToken = await framework.loadSuperToken(
                        arg.superTokenAddress
                    );
                    const [realtimeBalance, netFlow] = await Promise.all([
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
                    ]);
                    return {
                        data: typeGuard<GetRealtimeBalanceResult>({
                            availableBalanceWei:
                                realtimeBalance.availableBalance.toString(),
                            depositWei: realtimeBalance.deposit.toString(),
                            owedDepositWei:
                                realtimeBalance.owedDeposit.toString(),
                            timestamp: Math.floor(
                                realtimeBalance.timestamp.getTime() / 1000
                            ),
                            netFlowRateWei: netFlow,
                        }),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
