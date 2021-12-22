import {
    AccountTokenSnapshot,
    AccountTokenSnapshotGetQuery,
    AccountTokenSnapshotListQuery,
    AccountTokenSnapshotQueryHandler,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export interface GetAccountAccountTokenSnapshot extends BaseQuery2, AccountTokenSnapshotGetQuery {}

export interface ListAccountTokenSnapshots extends BaseQuery2, AccountTokenSnapshotListQuery {}

export const addAccountTokenEndpoints = (builder: SfEndpointBuilder) => ({
    accountTokenSnapshot: builder.query<AccountTokenSnapshot | undefined, GetAccountAccountTokenSnapshot>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new AccountTokenSnapshotQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    accountTokenSnapshots: builder.query<PagedResult<AccountTokenSnapshot>, ListAccountTokenSnapshots>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new AccountTokenSnapshotQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
