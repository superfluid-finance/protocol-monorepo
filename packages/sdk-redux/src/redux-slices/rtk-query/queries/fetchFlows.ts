
import {NetworkName} from "@superfluid-finance/sdk-core";
import request, { gql } from 'graphql-request';

import {
    Flow,
    initializedSuperfluidFrameworkSource,
} from '../../../superfluidApi';
import { rtkQuerySlice } from '../rtkQuerySlice';

// TODO: Get rid of this from here.
const subgraphUrls: { [key: string]: string } = {
    goerli: 'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli',
};

export interface FetchFlowsArg {
    networkName: NetworkName;
    accountAddress: string;
}

const extendedApi = rtkQuerySlice.injectEndpoints({
    endpoints: (builder) => ({
        fetchFlows: builder.query<Flow[], FetchFlowsArg>({
            queryFn: async (arg) => {
                const subgraphResponse = await request<{
                    accountTokenSnapshots: {
                        token: {
                            id: string;
                        };
                    }[];
                }>(
                    subgraphUrls[arg.networkName],
                    gql`
                        query ($accountAddress: String) {
                            accountTokenSnapshots(
                                where: { account: $accountAddress }
                            ) {
                                token {
                                    id
                                }
                            }
                        }
                    `,
                    {
                        accountAddress: arg.accountAddress.toLowerCase(),
                    }
                );

                const framework =
                    await initializedSuperfluidFrameworkSource.getForRead(
                        arg.networkName
                    );

                const userFlowsPromises: Promise<Flow[]>[] =
                    subgraphResponse.accountTokenSnapshots
                        .map((x) => x.token.id)
                        .map(async (tokenId) =>
                            framework
                                .cfa!.listFlows({
                                    superToken: tokenId,
                                    account: arg.accountAddress,
                                    onlyInFlows: false,
                                    onlyOutFlows: false,
                                })
                                .then((x) => [...x.inFlows, ...x.outFlows])
                                .then((flows) =>
                                    flows.map(
                                        (x) =>
                                            ({
                                                sender: x.sender,
                                                receiver: x.receiver,
                                                flowRate: x.flowRate,
                                                superToken: tokenId,
                                            } as Flow)
                                    )
                                )
                        );

                const allUserFlows = await Promise.all(userFlowsPromises).then(
                    (arrayOfArrays) => arrayOfArrays.flat()
                );

                return {
                    data: allUserFlows,
                };
            },
            providesTags: (_1, _2, arg) => [
                {
                    type: 'Flow',
                    id: `${arg.networkName}_${arg.accountAddress}`,
                },
            ],
        }),
    }),
    overrideExisting: false,
});

export const { useFetchFlowsQuery, useLazyFetchFlowsQuery } = extendedApi;
