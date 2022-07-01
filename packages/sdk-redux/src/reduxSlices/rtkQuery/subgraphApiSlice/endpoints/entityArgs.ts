import {
    AccountListQuery,
    AccountTokenSnapshotListQuery,
    AccountTokenSnapshotLogListQuery,
    IndexListQuery,
    IndexSubscriptionsListQuery,
    StreamListQuery,
    StreamPeriodListQuery,
    SubgraphGetQuery,
    TokenListQuery,
    TokenStatisticListQuery,
    TokenStatisticLogListQuery,
} from '@superfluid-finance/sdk-core';

export interface AccountTokenSnapshotQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface AccountTokenSnapshotLogQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface AccountTokenSnapshotsQuery extends AccountTokenSnapshotListQuery {
    chainId: number;
}

export interface AccountTokenSnapshotLogsQuery extends AccountTokenSnapshotLogListQuery {
    chainId: number;
}

export interface AccountQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface AccountsQuery extends AccountListQuery {
    chainId: number;
}

export interface IndexQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface IndexesQuery extends IndexListQuery {
    chainId: number;
}

export interface IndexSubscriptionQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface IndexSubscriptionsQuery extends IndexSubscriptionsListQuery {
    chainId: number;
}

export interface StreamQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface StreamsQuery extends StreamListQuery {
    chainId: number;
}

export interface StreamPeriodQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface StreamPeriodsQuery extends StreamPeriodListQuery {
    chainId: number;
}

export interface TokenQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface TokensQuery extends TokenListQuery {
    chainId: number;
}

export interface TokenStatisticQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface TokenStatisticsQuery extends TokenStatisticListQuery {
    chainId: number;
}

export interface TokenStatisticLogQuery extends SubgraphGetQuery {
    chainId: number;
}

export interface TokenStatisticLogsQuery extends TokenStatisticLogListQuery {
    chainId: number;
}
