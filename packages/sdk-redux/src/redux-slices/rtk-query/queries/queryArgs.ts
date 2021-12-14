import {NothingBoolean, NothingNumber, NothingString, PaginatedQueryArg, QueryArg} from '../../argTypes';

export type GetAllowanceForUpgradeToSuperTokenArg = QueryArg & {
    accountAddress: string;
    superTokenAddress: string;
};

export type GetIndexArg = QueryArg & {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
};

export type GetIndexSubscriptionsArg = QueryArg & {
    superTokenAddress: string;
    publisherAddress: string;
    indexId: string;
    subscriberAddress: string;
};

// TODO(KK): Clean up the timestamp flooring here...
// TODO(KK): Consider keeping netflow separate after all...

export type GetRealtimeBalanceArg = QueryArg & {
    superTokenAddress: string;
    accountAddress: string;
    estimationTimestamp: number | NothingNumber;
};

export type GetRealtimeBalanceResult = {
    availableBalanceWei: string;
    netFlowRateWei: string;
    depositWei: string;
    owedDepositWei: string;
    timestamp: number;
};

export type ListEventsArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    timestamp_gt: number | NothingNumber;
};

export type ListIndexesArg = PaginatedQueryArg & {
    indexId: string | NothingString;
    publisherAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export type ListIndexSubscriptionsArg = PaginatedQueryArg & {
    subscriberAddress: string | NothingString;
    approved: boolean | NothingBoolean;
};

export type ListStreamsArg = PaginatedQueryArg & {
    senderAddress: string | NothingString;
    receiverAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};

export type ListSuperTokensArg = PaginatedQueryArg & {
    isListed: boolean | NothingBoolean;
};

export type ListUserInteractedSuperTokensArg = PaginatedQueryArg & {
    accountAddress: string | NothingString;
    superTokenAddress: string | NothingString;
};
