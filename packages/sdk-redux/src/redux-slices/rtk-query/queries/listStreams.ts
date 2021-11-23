import { IStream, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedContext } from '../../../createSdkReduxParts';
import { NothingString, PaginatedQueryArg } from '../../argTypes';
import { rtkQuerySlice } from '../rtkQuerySlice';
import { getMostSpecificStreamTag } from '../cacheTags/streamTags';

export type ListStreamsArg = PaginatedQueryArg & {
    senderAddress: string | NothingString;
    receiverAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export const { useListStreamsQuery, useLazyListStreamsQuery } =
    rtkQuerySlice.injectEndpoints({
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
                    const framework = await initializedContext.getFramework(
                        arg.chainId
                    );

                    return {
                        data: await framework.query.listStreams(
                            {
                                sender: arg.senderAddress,
                                receiver: arg.receiverAddress,
                                token: arg.superTokenAddress,
                            },
                            new Paging(arg)
                        ),
                    };
                },
            }),
        }),
        overrideExisting: false,
    });
