import { IStream, PagedResult, Paging } from '@superfluid-finance/sdk-core';

import { initializedSuperfluidSource } from '../../../superfluidApi';
import {NothingString, PaginatedQueryArg} from '../../baseArg';
import { rtkQuerySlice, streamTag } from '../rtkQuerySlice';
import { insertIf } from '../../../utils';

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
                    ...insertIf(
                        !(
                            arg.senderAddress ||
                            arg.receiverAddress ||
                            arg.superTokenAddress
                        ),
                        streamTag(arg.chainId)
                    ),
                    ...insertIf(
                        arg.senderAddress,
                        streamTag(arg.chainId, arg.senderAddress!)
                    ),
                    ...insertIf(
                        arg.receiverAddress,
                        streamTag(arg.chainId, arg.receiverAddress!)
                    ),
                    ...insertIf(
                        arg.superTokenAddress,
                        streamTag(arg.chainId, arg.superTokenAddress!)
                    ),
                ],
                queryFn: async (arg) => {
                    const framework =
                        await initializedSuperfluidSource.getFramework(
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
