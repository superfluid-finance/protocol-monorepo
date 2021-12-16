import {
    createSkipPaging,
    IStream,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getSfContext} from '../../../createSdkReduxParts';
import {NothingString, PaginatedQueryArg} from '../../argTypes';
import {getMostSpecificStreamTag} from '../cacheTags/streamTags';
import {rtkQuerySlice} from '../rtkQuerySlice';

export type ListStreamsArg = PaginatedQueryArg & {
    senderAddress: string | NothingString;
    receiverAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

const apiSlice = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        listStreams: builder.query<PagedResult<IStream>, ListStreamsArg>({
            providesTags: (_result, _error, arg) => [
                getMostSpecificStreamTag({
                    chainId: arg.chainId,
                    address1: arg.superTokenAddress,
                    address3: arg.receiverAddress,
                    address2: arg.senderAddress,
                }),
            ],
            queryFn: async (arg) => {
                const framework = await getSfContext().getFramework(
                    arg.chainId
                );

                return {
                    data: await framework.query.listStreams(
                        {
                            sender: arg.senderAddress,
                            receiver: arg.receiverAddress,
                            token: arg.superTokenAddress,
                        },
                        createSkipPaging(arg)
                    ),
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
    useListStreamsQuery,
    /**
     * @category React Hooks
     */
    useLazyListStreamsQuery,
} = apiSlice;
