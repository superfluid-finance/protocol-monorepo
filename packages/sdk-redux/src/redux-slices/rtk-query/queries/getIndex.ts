import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { IWeb3Index } from '@superfluid-finance/sdk-core';

export type GetIndexArg = QueryArg & {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
};

export const {
    useGetIndexSubscriptionQuery,
    useLazyGetIndexSubscriptionQuery,
} = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getIndexSubscription: builder.query<IWeb3Index, GetIndexArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );
                const index = await superToken.getIndex({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    providerOrSigner: framework.settings.provider,
                });
                return {
                    data: index,
                };
            },
        }),
    }),
    overrideExisting: false,
});
