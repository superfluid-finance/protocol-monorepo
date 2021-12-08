import {IWeb3Subscription} from '@superfluid-finance/sdk-core';

import {getSfContext} from '../../../createSdkReduxParts';
import {QueryArg} from '../../argTypes';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

export type GetIndexSubscriptionArg = QueryArg & {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getIndexSubscription: builder.query<
            IWeb3Subscription,
            GetIndexSubscriptionArg
        >({
            providesTags: (_result, _error, arg) => [
                getMostSpecificIndexTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address2: arg.publisherAddress,
                    address3: arg.subscriberAddress,
                    indexId: arg.indexId,
                }),
            ],
            queryFn: async (arg) => {
                const framework = await getSfContext().getFramework(
                    arg.chainId
                );
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

export const {
    endpoints: {getIndexSubscription},
    useGetIndexSubscriptionQuery,
    useLazyGetIndexSubscriptionQuery,
} = apiSlice;
