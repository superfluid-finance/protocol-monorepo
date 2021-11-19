import { IWeb3Subscription } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import { QueryArg } from '../../baseArg';
import { indexTag, rtkQuerySlice} from '../rtkQuerySlice';

export type GetIndexSubscriptionArg = QueryArg & {
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
        getIndexSubscription: builder.query<
            IWeb3Subscription,
            GetIndexSubscriptionArg
        >({
            providesTags: (_1, _2, arg) => [
                indexTag(arg.chainId, arg.superTokenAddress, arg.publisherAddress, arg.indexId, arg.subscriberAddress),
            ],
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidSource.getFramework(arg.chainId);
                const superToken = await framework.loadSuperToken(
                    arg.superTokenAddress
                );
                const indexSubscription = await superToken.getSubscription({
                    indexId: arg.indexId,
                    publisher: arg.publisherAddress,
                    subscriber: arg.subscriberAddress,
                    providerOrSigner: framework.settings.provider,
                });
                return {
                    data: indexSubscription,
                };
            },
        }),
    }),
    overrideExisting: false,
});
