import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingNumber, QueryArg} from '../../baseArg';
import { indexTag, rtkQuerySlice, streamTag, tokenTag} from '../rtkQuerySlice';
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
                providesTags: (_1, _2, arg) => [
                    indexTag(arg.chainId, arg.superTokenAddress, arg.accountAddress),
                    streamTag(arg.chainId, arg.superTokenAddress, arg.accountAddress),
                    tokenTag(arg.chainId, arg.superTokenAddress, arg.accountAddress),
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
