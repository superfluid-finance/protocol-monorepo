import {
    Account,
    AccountListQuery,
    AccountTokenSnapshot,
    AccountTokenSnapshotListQuery,
    Index,
    IndexListQuery,
    IndexSubscription,
    IndexSubscriptionsListQuery,
    IndexUpdatedEvent,
    IndexUpdatedEventListQuery,
    Stream,
    StreamListQuery,
    StreamPeriod,
    StreamPeriodListQuery,
    SubscriptionUnitsUpdatedEvent,
    SubscriptionUnitsUpdatedEventListQuery,
    Token,
    TokenListQuery,
    TokenStatistic,
    TokenStatisticListQuery,
} from '@superfluid-finance/sdk-core';

import {BaseGetQuery, BaseQuery2} from '../../argTypes';

export type AccountTokenSnapshotQuery = BaseGetQuery<AccountTokenSnapshot>;

export interface AccountTokenSnapshotsQuery extends BaseQuery2, AccountTokenSnapshotListQuery {}

export type AccountQuery = BaseGetQuery<Account>;

export interface AccountsQuery extends BaseQuery2, AccountListQuery {}

export type IndexQuery = BaseGetQuery<Index>;

export interface IndexesQuery extends BaseQuery2, IndexListQuery {}

export type IndexSubscriptionQuery = BaseGetQuery<IndexSubscription>;

export interface IndexSubscriptionsQuery extends BaseQuery2, IndexSubscriptionsListQuery {}

export type StreamQuery = BaseGetQuery<Stream>;

export interface StreamsQuery extends BaseQuery2, StreamListQuery {}

export type StreamPeriodQuery = BaseGetQuery<StreamPeriod>;

export interface StreamPeriodsQuery extends BaseQuery2, StreamPeriodListQuery {}

export type TokenQuery = BaseGetQuery<Token>;

export interface TokensQuery extends BaseQuery2, TokenListQuery {}

export type TokenStatisticQuery = BaseGetQuery<TokenStatistic>;

export interface TokenStatisticsQuery extends BaseQuery2, TokenStatisticListQuery {}

export type IndexUpdatedEventQuery = BaseGetQuery<IndexUpdatedEvent>;

export interface IndexUpdatedEventsQuery extends BaseQuery2, IndexUpdatedEventListQuery {}

export type SubscriptionUnitsUpdatedEventQuery = BaseGetQuery<SubscriptionUnitsUpdatedEvent>;

export interface SubscriptionUnitsUpdatedEventsQuery extends BaseQuery2, SubscriptionUnitsUpdatedEventListQuery {}
