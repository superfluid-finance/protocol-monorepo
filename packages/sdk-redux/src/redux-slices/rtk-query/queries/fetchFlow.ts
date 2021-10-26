import {
    FlowDetails,
    initializedSuperfluidFrameworkSource,
} from '../../../superfluidApi';
import { rtkQuerySlice } from '../rtkQuerySlice';

export interface FetchFlowArg {
    networkName: string;
    superToken: string;
    sender: string;
    receiver: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        fetchFlow: builder.query<FlowDetails, FetchFlowArg>({
            queryFn: async (arg) => {
                const framework =
                    await initializedSuperfluidFrameworkSource.getForRead(
                        arg.networkName
                    );
                const flow = await framework.cfa!.getFlow({
                    superToken: arg.superToken,
                    sender: arg.sender,
                    receiver: arg.receiver,
                });
                return {
                    data: { ...flow, timestamp: flow.timestamp.getTime() },
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.superToken}_${arg.sender}_${arg.receiver}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});

export const { useFetchFlowQuery, useLazyFetchFlowQuery } = extendedApi;
