import {BaseQueryFn} from '@reduxjs/toolkit/dist/query';

export default function subgraphApiBaseQuery(): BaseQueryFn<void, unknown, unknown, Record<string, unknown>> {
    return function () {
        throw new Error('All queries & mutations must use the `queryFn` definition syntax.');
    };
}

export type SubgraphApiBaseQuery = ReturnType<typeof subgraphApiBaseQuery>;
