import {
    Account,
    AccountGetQuery,
    AccountListQuery,
    AccountQueryHandler,
    PagedResult,
} from '@superfluid-finance/sdk-core';

import {getFramework} from '../../../sdkReduxConfig';
import {BaseQuery2} from '../../argTypes';
import {SfEndpointBuilder} from '../baseQuery';

export interface GetAccount extends BaseQuery2, AccountGetQuery {}

export interface ListAccounts extends BaseQuery2, AccountListQuery {}

export const addAccountEndpoints = (builder: SfEndpointBuilder) => ({
    account: builder.query<Account | undefined, GetAccount>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new AccountQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.get(query);
            return {
                data: result,
            };
        },
    }),
    accounts: builder.query<PagedResult<Account>, ListAccounts>({
        queryFn: async (arg) => {
            const framework = await getFramework(arg.chainId);
            const handler = new AccountQueryHandler(framework.query.subgraphClient);
            const {chainId, ...query} = arg;
            const result = await handler.list(query);
            return {
                data: result,
            };
        },
    }),
});
