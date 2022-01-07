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

export type GetAccountTokenSnapshot = BaseGetQuery<AccountTokenSnapshot>;

export interface ListAccountTokenSnapshots extends BaseQuery2, AccountTokenSnapshotListQuery {}

export type GetAccount = BaseGetQuery<Account>;

export interface ListAccounts extends BaseQuery2, AccountListQuery {}

export type GetIndex2 = BaseGetQuery<Index>;

export interface ListIndexes2 extends BaseQuery2, IndexListQuery {}

export type GetIndexSubscription = BaseGetQuery<IndexSubscription>;

export interface ListIndexSubscriptions2 extends BaseQuery2, IndexSubscriptionsListQuery {}

export type GetStream2 = BaseGetQuery<Stream>;

export interface ListStreams2 extends BaseQuery2, StreamListQuery {}

export type GetStreamPeriod = BaseGetQuery<StreamPeriod>;

export interface ListStreamPeriods extends BaseQuery2, StreamPeriodListQuery {}

export type GetToken = BaseGetQuery<Token>;

export interface ListTokens extends BaseQuery2, TokenListQuery {}

export type GetTokenStatistic = BaseGetQuery<TokenStatistic>;

export interface ListTokenStatistics extends BaseQuery2, TokenStatisticListQuery {}

export type GetIndexUpdatedEvent = BaseGetQuery<IndexUpdatedEvent>;

export interface ListIndexUpdatedEvents extends BaseQuery2, IndexUpdatedEventListQuery {}

export type GetSubscriptionUnitsUpdatedEvent = BaseGetQuery<SubscriptionUnitsUpdatedEvent>;

export interface ListSubscriptionUnitsUpdatedEvents extends BaseQuery2, SubscriptionUnitsUpdatedEventListQuery {}
