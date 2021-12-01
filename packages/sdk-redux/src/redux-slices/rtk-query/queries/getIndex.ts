import {IWeb3Index} from '@superfluid-finance/sdk-core';

import {initializedSuperfluidContext} from '../../../createSdkReduxParts';
import {QueryArg} from '../../argTypes';
import {getMostSpecificIndexTag} from '../cacheTags/indexTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

export type GetIndexArg = QueryArg & {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        getIndexSubscription: builder.query<IWeb3Index, GetIndexArg>({
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
                const framework =
                    await initializedSuperfluidContext.getFramework(
                        arg.chainId
                    );
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

export const {
    /**
     * @category React Hooks
     */
    useGetIndexSubscriptionQuery,
    /**
     * @category React Hooks
     */
    useLazyGetIndexSubscriptionQuery,
} = apiSlice;
