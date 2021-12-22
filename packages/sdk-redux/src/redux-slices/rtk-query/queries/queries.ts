import {
    AllEvents,
    IIndex,
    IIndexSubscription,
    ILightAccountTokenSnapshot,
    IStream,
    ISuperToken,
    IWeb3Index,
    IWeb3Subscription,
} from '@superfluid-finance/sdk-core';

import {BasePaginatedQuery, BaseQuery, NothingBoolean, NothingNumber, NothingString} from '../../argTypes';

export interface GetAllowanceForUpgradeToSuperToken extends BaseQuery<string> {
    accountAddress: string;
    superTokenAddress: string;
}

export interface GetIndex extends BaseQuery<IWeb3Index> {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
}

export interface GetIndexSubscriptions extends BaseQuery<IWeb3Subscription> {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
}

// TODO(KK): Clean up the timestamp flooring here...
// TODO(KK): Consider keeping netflow separate after all...

export interface GetRealtimeBalance extends BaseQuery<GetRealtimeBalanceResult> {
    superTokenAddress: string;
    accountAddress: string;
    estimationTimestamp: number | NothingNumber;
}

export interface GetRealtimeBalanceResult {
    availableBalanceWei: string;
    netFlowRateWei: string;
    depositWei: string;
    owedDepositWei: string;
    timestamp: number;
}

export interface ListEvents extends BasePaginatedQuery<AllEvents> {
    accountAddress: string | NothingString;
    timestamp_gt: number | NothingNumber;
}

export interface ListIndexes extends BasePaginatedQuery<IIndex> {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
}

export interface ListIndexSubscriptions extends BasePaginatedQuery<IIndexSubscription> {
    subscriberAddress: string | NothingString;
    approved: boolean | NothingBoolean;
}

export interface ListStreams extends BasePaginatedQuery<IStream> {
    senderAddress: string | NothingString;
    receiverAddress: string | NothingString;
    superTokenAddress: string | NothingString;
}

export interface ListSuperTokens extends BasePaginatedQuery<ISuperToken> {
    isListed: boolean | NothingBoolean;
}

export interface ListUserInteractedSuperTokens extends BasePaginatedQuery<ILightAccountTokenSnapshot> {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
}
