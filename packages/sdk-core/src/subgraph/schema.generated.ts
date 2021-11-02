export type Maybe<T> = T | null;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
  BigDecimal: string;
  BigInt: string;
  Bytes: string;
};

/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type Account = {
  __typename?: 'Account';
  accountTokenSnapshots: Array<AccountTokenSnapshot>;
  createdAtBlockNumber: Scalars['BigInt'];
  createdAtTimestamp: Scalars['BigInt'];
  id: Scalars['ID'];
  inflows: Array<Stream>;
  /** Indicates whether the address/account is a super app. */
  isSuperApp: Scalars['Boolean'];
  outflows: Array<Stream>;
  publishedIndexes: Array<Index>;
  receivedTransferEvents: Array<TransferEvent>;
  sentTransferEvents: Array<TransferEvent>;
  subscriptions: Array<IndexSubscription>;
  tokenDowngradedEvents: Array<TokenDowngradedEvent>;
  tokenUpgradedEvents: Array<TokenUpgradedEvent>;
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountAccountTokenSnapshotsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AccountTokenSnapshot_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<AccountTokenSnapshot_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountInflowsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Stream_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<Stream_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountOutflowsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Stream_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<Stream_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountPublishedIndexesArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Index_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<Index_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountReceivedTransferEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TransferEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<TransferEvent_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountSentTransferEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TransferEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<TransferEvent_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountSubscriptionsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscription_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexSubscription_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountTokenDowngradedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenDowngradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<TokenDowngradedEvent_Filter>;
};


/**
 * Account: A HOL entity created for any addresses which interact with
 * Superfluid contracts.
 */
export type AccountTokenUpgradedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenUpgradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<TokenUpgradedEvent_Filter>;
};

/**
 * AccountTokenSnapshot: An aggregate entity which aggregates data on an account's
 * interaction with a `token`.
 */
export type AccountTokenSnapshot = {
  __typename?: 'AccountTokenSnapshot';
  account: Account;
  /** Balance of `account` as of `updatedAtTimestamp`/`updatedAtBlock`. */
  balanceUntilUpdatedAt: Scalars['BigInt'];
  /** ID composed of: accountID-tokenID */
  id: Scalars['ID'];
  token: Token;
  /**
   * The total amount of `token` streamed to this `account` until
   * the `updatedAtTimestamp`/`updatedAtBlock`.
   */
  totalAmountStreamedUntilUpdatedAt: Scalars['BigInt'];
  /** The total amount of `token` this `account` has transferred. */
  totalAmountTransferredUntilUpdatedAt: Scalars['BigInt'];
  /** Counts all currently (as of updatedAt) approved subscriptions whether or not they have units. */
  totalApprovedSubscriptions: Scalars['Int'];
  /** The total inflow rate (receive flowRate per second) of the `account`. */
  totalInflowRate: Scalars['BigInt'];
  /** The total net flow rate of the `account` as of `updatedAtTimestamp`/`updatedAtBlock`. */
  totalNetFlowRate: Scalars['BigInt'];
  /** The number of currently open streams. */
  totalNumberOfActiveStreams: Scalars['Int'];
  /** The number of all-time closed streams. */
  totalNumberOfClosedStreams: Scalars['Int'];
  /** The total outflow rate (send flowrate per second) of the `account`. */
  totalOutflowRate: Scalars['BigInt'];
  /** The current (as of updatedAt) number of subscriptions with units allocated to them tied to this `account`. */
  totalSubscriptionsWithUnits: Scalars['Int'];
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};

export type AccountTokenSnapshot_Filter = {
  account?: Maybe<Scalars['String']>;
  account_contains?: Maybe<Scalars['String']>;
  account_ends_with?: Maybe<Scalars['String']>;
  account_gt?: Maybe<Scalars['String']>;
  account_gte?: Maybe<Scalars['String']>;
  account_in?: Maybe<Array<Scalars['String']>>;
  account_lt?: Maybe<Scalars['String']>;
  account_lte?: Maybe<Scalars['String']>;
  account_not?: Maybe<Scalars['String']>;
  account_not_contains?: Maybe<Scalars['String']>;
  account_not_ends_with?: Maybe<Scalars['String']>;
  account_not_in?: Maybe<Array<Scalars['String']>>;
  account_not_starts_with?: Maybe<Scalars['String']>;
  account_starts_with?: Maybe<Scalars['String']>;
  balanceUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  balanceUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  balanceUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  token?: Maybe<Scalars['String']>;
  token_contains?: Maybe<Scalars['String']>;
  token_ends_with?: Maybe<Scalars['String']>;
  token_gt?: Maybe<Scalars['String']>;
  token_gte?: Maybe<Scalars['String']>;
  token_in?: Maybe<Array<Scalars['String']>>;
  token_lt?: Maybe<Scalars['String']>;
  token_lte?: Maybe<Scalars['String']>;
  token_not?: Maybe<Scalars['String']>;
  token_not_contains?: Maybe<Scalars['String']>;
  token_not_ends_with?: Maybe<Scalars['String']>;
  token_not_in?: Maybe<Array<Scalars['String']>>;
  token_not_starts_with?: Maybe<Scalars['String']>;
  token_starts_with?: Maybe<Scalars['String']>;
  totalAmountStreamedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountStreamedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountTransferredUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountTransferredUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalApprovedSubscriptions?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_gt?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_gte?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_in?: Maybe<Array<Scalars['Int']>>;
  totalApprovedSubscriptions_lt?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_lte?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_not?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_not_in?: Maybe<Array<Scalars['Int']>>;
  totalInflowRate?: Maybe<Scalars['BigInt']>;
  totalInflowRate_gt?: Maybe<Scalars['BigInt']>;
  totalInflowRate_gte?: Maybe<Scalars['BigInt']>;
  totalInflowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalInflowRate_lt?: Maybe<Scalars['BigInt']>;
  totalInflowRate_lte?: Maybe<Scalars['BigInt']>;
  totalInflowRate_not?: Maybe<Scalars['BigInt']>;
  totalInflowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalNetFlowRate?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_gt?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_gte?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalNetFlowRate_lt?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_lte?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_not?: Maybe<Scalars['BigInt']>;
  totalNetFlowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalNumberOfActiveStreams?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_gt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_gte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfActiveStreams_lt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_lte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_not?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_not_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfClosedStreams?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_gt?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_gte?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfClosedStreams_lt?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_lte?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_not?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_not_in?: Maybe<Array<Scalars['Int']>>;
  totalOutflowRate?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_gt?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_gte?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalOutflowRate_lt?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_lte?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_not?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSubscriptionsWithUnits?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_in?: Maybe<Array<Scalars['Int']>>;
  totalSubscriptionsWithUnits_lt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_lte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not_in?: Maybe<Array<Scalars['Int']>>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum AccountTokenSnapshot_OrderBy {
  Account = 'account',
  BalanceUntilUpdatedAt = 'balanceUntilUpdatedAt',
  Id = 'id',
  Token = 'token',
  TotalAmountStreamedUntilUpdatedAt = 'totalAmountStreamedUntilUpdatedAt',
  TotalAmountTransferredUntilUpdatedAt = 'totalAmountTransferredUntilUpdatedAt',
  TotalApprovedSubscriptions = 'totalApprovedSubscriptions',
  TotalInflowRate = 'totalInflowRate',
  TotalNetFlowRate = 'totalNetFlowRate',
  TotalNumberOfActiveStreams = 'totalNumberOfActiveStreams',
  TotalNumberOfClosedStreams = 'totalNumberOfClosedStreams',
  TotalOutflowRate = 'totalOutflowRate',
  TotalSubscriptionsWithUnits = 'totalSubscriptionsWithUnits',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type Account_Filter = {
  createdAtBlockNumber?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isSuperApp?: Maybe<Scalars['Boolean']>;
  isSuperApp_in?: Maybe<Array<Scalars['Boolean']>>;
  isSuperApp_not?: Maybe<Scalars['Boolean']>;
  isSuperApp_not_in?: Maybe<Array<Scalars['Boolean']>>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum Account_OrderBy {
  AccountTokenSnapshots = 'accountTokenSnapshots',
  CreatedAtBlockNumber = 'createdAtBlockNumber',
  CreatedAtTimestamp = 'createdAtTimestamp',
  Id = 'id',
  Inflows = 'inflows',
  IsSuperApp = 'isSuperApp',
  Outflows = 'outflows',
  PublishedIndexes = 'publishedIndexes',
  ReceivedTransferEvents = 'receivedTransferEvents',
  SentTransferEvents = 'sentTransferEvents',
  Subscriptions = 'subscriptions',
  TokenDowngradedEvents = 'tokenDowngradedEvents',
  TokenUpgradedEvents = 'tokenUpgradedEvents',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type AgreementClassRegisteredEvent = Event & {
  __typename?: 'AgreementClassRegisteredEvent';
  agreementType: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  code: Scalars['Bytes'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type AgreementClassRegisteredEvent_Filter = {
  agreementType?: Maybe<Scalars['Bytes']>;
  agreementType_contains?: Maybe<Scalars['Bytes']>;
  agreementType_in?: Maybe<Array<Scalars['Bytes']>>;
  agreementType_not?: Maybe<Scalars['Bytes']>;
  agreementType_not_contains?: Maybe<Scalars['Bytes']>;
  agreementType_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  code?: Maybe<Scalars['Bytes']>;
  code_contains?: Maybe<Scalars['Bytes']>;
  code_in?: Maybe<Array<Scalars['Bytes']>>;
  code_not?: Maybe<Scalars['Bytes']>;
  code_not_contains?: Maybe<Scalars['Bytes']>;
  code_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum AgreementClassRegisteredEvent_OrderBy {
  AgreementType = 'agreementType',
  BlockNumber = 'blockNumber',
  Code = 'code',
  Id = 'id',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type AgreementClassUpdatedEvent = Event & {
  __typename?: 'AgreementClassUpdatedEvent';
  agreementType: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  code: Scalars['Bytes'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type AgreementClassUpdatedEvent_Filter = {
  agreementType?: Maybe<Scalars['Bytes']>;
  agreementType_contains?: Maybe<Scalars['Bytes']>;
  agreementType_in?: Maybe<Array<Scalars['Bytes']>>;
  agreementType_not?: Maybe<Scalars['Bytes']>;
  agreementType_not_contains?: Maybe<Scalars['Bytes']>;
  agreementType_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  code?: Maybe<Scalars['Bytes']>;
  code_contains?: Maybe<Scalars['Bytes']>;
  code_in?: Maybe<Array<Scalars['Bytes']>>;
  code_not?: Maybe<Scalars['Bytes']>;
  code_not_contains?: Maybe<Scalars['Bytes']>;
  code_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum AgreementClassUpdatedEvent_OrderBy {
  AgreementType = 'agreementType',
  BlockNumber = 'blockNumber',
  Code = 'code',
  Id = 'id',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type AgreementLiquidatedByEvent = Event & {
  __typename?: 'AgreementLiquidatedByEvent';
  agreementClass: Scalars['Bytes'];
  agreementId: Scalars['Bytes'];
  bailoutAmount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  bondAccount: Scalars['Bytes'];
  id: Scalars['ID'];
  liquidatorAccount: Scalars['Bytes'];
  penaltyAccount: Scalars['Bytes'];
  rewardAmount: Scalars['BigInt'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type AgreementLiquidatedByEvent_Filter = {
  agreementClass?: Maybe<Scalars['Bytes']>;
  agreementClass_contains?: Maybe<Scalars['Bytes']>;
  agreementClass_in?: Maybe<Array<Scalars['Bytes']>>;
  agreementClass_not?: Maybe<Scalars['Bytes']>;
  agreementClass_not_contains?: Maybe<Scalars['Bytes']>;
  agreementClass_not_in?: Maybe<Array<Scalars['Bytes']>>;
  agreementId?: Maybe<Scalars['Bytes']>;
  agreementId_contains?: Maybe<Scalars['Bytes']>;
  agreementId_in?: Maybe<Array<Scalars['Bytes']>>;
  agreementId_not?: Maybe<Scalars['Bytes']>;
  agreementId_not_contains?: Maybe<Scalars['Bytes']>;
  agreementId_not_in?: Maybe<Array<Scalars['Bytes']>>;
  bailoutAmount?: Maybe<Scalars['BigInt']>;
  bailoutAmount_gt?: Maybe<Scalars['BigInt']>;
  bailoutAmount_gte?: Maybe<Scalars['BigInt']>;
  bailoutAmount_in?: Maybe<Array<Scalars['BigInt']>>;
  bailoutAmount_lt?: Maybe<Scalars['BigInt']>;
  bailoutAmount_lte?: Maybe<Scalars['BigInt']>;
  bailoutAmount_not?: Maybe<Scalars['BigInt']>;
  bailoutAmount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  bondAccount?: Maybe<Scalars['Bytes']>;
  bondAccount_contains?: Maybe<Scalars['Bytes']>;
  bondAccount_in?: Maybe<Array<Scalars['Bytes']>>;
  bondAccount_not?: Maybe<Scalars['Bytes']>;
  bondAccount_not_contains?: Maybe<Scalars['Bytes']>;
  bondAccount_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  liquidatorAccount?: Maybe<Scalars['Bytes']>;
  liquidatorAccount_contains?: Maybe<Scalars['Bytes']>;
  liquidatorAccount_in?: Maybe<Array<Scalars['Bytes']>>;
  liquidatorAccount_not?: Maybe<Scalars['Bytes']>;
  liquidatorAccount_not_contains?: Maybe<Scalars['Bytes']>;
  liquidatorAccount_not_in?: Maybe<Array<Scalars['Bytes']>>;
  penaltyAccount?: Maybe<Scalars['Bytes']>;
  penaltyAccount_contains?: Maybe<Scalars['Bytes']>;
  penaltyAccount_in?: Maybe<Array<Scalars['Bytes']>>;
  penaltyAccount_not?: Maybe<Scalars['Bytes']>;
  penaltyAccount_not_contains?: Maybe<Scalars['Bytes']>;
  penaltyAccount_not_in?: Maybe<Array<Scalars['Bytes']>>;
  rewardAmount?: Maybe<Scalars['BigInt']>;
  rewardAmount_gt?: Maybe<Scalars['BigInt']>;
  rewardAmount_gte?: Maybe<Scalars['BigInt']>;
  rewardAmount_in?: Maybe<Array<Scalars['BigInt']>>;
  rewardAmount_lt?: Maybe<Scalars['BigInt']>;
  rewardAmount_lte?: Maybe<Scalars['BigInt']>;
  rewardAmount_not?: Maybe<Scalars['BigInt']>;
  rewardAmount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum AgreementLiquidatedByEvent_OrderBy {
  AgreementClass = 'agreementClass',
  AgreementId = 'agreementId',
  BailoutAmount = 'bailoutAmount',
  BlockNumber = 'blockNumber',
  BondAccount = 'bondAccount',
  Id = 'id',
  LiquidatorAccount = 'liquidatorAccount',
  PenaltyAccount = 'penaltyAccount',
  RewardAmount = 'rewardAmount',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

export type AppRegisteredEvent = Event & {
  __typename?: 'AppRegisteredEvent';
  app: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type AppRegisteredEvent_Filter = {
  app?: Maybe<Scalars['Bytes']>;
  app_contains?: Maybe<Scalars['Bytes']>;
  app_in?: Maybe<Array<Scalars['Bytes']>>;
  app_not?: Maybe<Scalars['Bytes']>;
  app_not_contains?: Maybe<Scalars['Bytes']>;
  app_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum AppRegisteredEvent_OrderBy {
  App = 'app',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type Block_Height = {
  hash?: Maybe<Scalars['Bytes']>;
  number?: Maybe<Scalars['Int']>;
};

export type BurnedEvent = Event & {
  __typename?: 'BurnedEvent';
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  data: Scalars['Bytes'];
  from: Scalars['Bytes'];
  id: Scalars['ID'];
  operator: Scalars['Bytes'];
  operatorData: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type BurnedEvent_Filter = {
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  data?: Maybe<Scalars['Bytes']>;
  data_contains?: Maybe<Scalars['Bytes']>;
  data_in?: Maybe<Array<Scalars['Bytes']>>;
  data_not?: Maybe<Scalars['Bytes']>;
  data_not_contains?: Maybe<Scalars['Bytes']>;
  data_not_in?: Maybe<Array<Scalars['Bytes']>>;
  from?: Maybe<Scalars['Bytes']>;
  from_contains?: Maybe<Scalars['Bytes']>;
  from_in?: Maybe<Array<Scalars['Bytes']>>;
  from_not?: Maybe<Scalars['Bytes']>;
  from_not_contains?: Maybe<Scalars['Bytes']>;
  from_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  operator?: Maybe<Scalars['Bytes']>;
  operatorData?: Maybe<Scalars['Bytes']>;
  operatorData_contains?: Maybe<Scalars['Bytes']>;
  operatorData_in?: Maybe<Array<Scalars['Bytes']>>;
  operatorData_not?: Maybe<Scalars['Bytes']>;
  operatorData_not_contains?: Maybe<Scalars['Bytes']>;
  operatorData_not_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_contains?: Maybe<Scalars['Bytes']>;
  operator_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_not?: Maybe<Scalars['Bytes']>;
  operator_not_contains?: Maybe<Scalars['Bytes']>;
  operator_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum BurnedEvent_OrderBy {
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Data = 'data',
  From = 'from',
  Id = 'id',
  Operator = 'operator',
  OperatorData = 'operatorData',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type CfAv1LiquidationPeriodChangedEvent = Event & {
  __typename?: 'CFAv1LiquidationPeriodChangedEvent';
  blockNumber: Scalars['BigInt'];
  host: Scalars['Bytes'];
  id: Scalars['ID'];
  isSet: Scalars['Boolean'];
  liquidationPeriod: Scalars['BigInt'];
  superToken: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type CfAv1LiquidationPeriodChangedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  host?: Maybe<Scalars['Bytes']>;
  host_contains?: Maybe<Scalars['Bytes']>;
  host_in?: Maybe<Array<Scalars['Bytes']>>;
  host_not?: Maybe<Scalars['Bytes']>;
  host_not_contains?: Maybe<Scalars['Bytes']>;
  host_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isSet?: Maybe<Scalars['Boolean']>;
  isSet_in?: Maybe<Array<Scalars['Boolean']>>;
  isSet_not?: Maybe<Scalars['Boolean']>;
  isSet_not_in?: Maybe<Array<Scalars['Boolean']>>;
  liquidationPeriod?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_gt?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_gte?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_in?: Maybe<Array<Scalars['BigInt']>>;
  liquidationPeriod_lt?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_lte?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_not?: Maybe<Scalars['BigInt']>;
  liquidationPeriod_not_in?: Maybe<Array<Scalars['BigInt']>>;
  superToken?: Maybe<Scalars['Bytes']>;
  superToken_contains?: Maybe<Scalars['Bytes']>;
  superToken_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken_not?: Maybe<Scalars['Bytes']>;
  superToken_not_contains?: Maybe<Scalars['Bytes']>;
  superToken_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum CfAv1LiquidationPeriodChangedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Host = 'host',
  Id = 'id',
  IsSet = 'isSet',
  LiquidationPeriod = 'liquidationPeriod',
  SuperToken = 'superToken',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type ConfigChangedEvent = Event & {
  __typename?: 'ConfigChangedEvent';
  blockNumber: Scalars['BigInt'];
  host: Scalars['Bytes'];
  id: Scalars['ID'];
  isSet: Scalars['Boolean'];
  key: Scalars['Bytes'];
  superToken: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
  value: Scalars['BigInt'];
};

export type ConfigChangedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  host?: Maybe<Scalars['Bytes']>;
  host_contains?: Maybe<Scalars['Bytes']>;
  host_in?: Maybe<Array<Scalars['Bytes']>>;
  host_not?: Maybe<Scalars['Bytes']>;
  host_not_contains?: Maybe<Scalars['Bytes']>;
  host_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isSet?: Maybe<Scalars['Boolean']>;
  isSet_in?: Maybe<Array<Scalars['Boolean']>>;
  isSet_not?: Maybe<Scalars['Boolean']>;
  isSet_not_in?: Maybe<Array<Scalars['Boolean']>>;
  key?: Maybe<Scalars['Bytes']>;
  key_contains?: Maybe<Scalars['Bytes']>;
  key_in?: Maybe<Array<Scalars['Bytes']>>;
  key_not?: Maybe<Scalars['Bytes']>;
  key_not_contains?: Maybe<Scalars['Bytes']>;
  key_not_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken?: Maybe<Scalars['Bytes']>;
  superToken_contains?: Maybe<Scalars['Bytes']>;
  superToken_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken_not?: Maybe<Scalars['Bytes']>;
  superToken_not_contains?: Maybe<Scalars['Bytes']>;
  superToken_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  value?: Maybe<Scalars['BigInt']>;
  value_gt?: Maybe<Scalars['BigInt']>;
  value_gte?: Maybe<Scalars['BigInt']>;
  value_in?: Maybe<Array<Scalars['BigInt']>>;
  value_lt?: Maybe<Scalars['BigInt']>;
  value_lte?: Maybe<Scalars['BigInt']>;
  value_not?: Maybe<Scalars['BigInt']>;
  value_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum ConfigChangedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Host = 'host',
  Id = 'id',
  IsSet = 'isSet',
  Key = 'key',
  SuperToken = 'superToken',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash',
  Value = 'value'
}

export type CustomSuperTokenCreatedEvent = Event & {
  __typename?: 'CustomSuperTokenCreatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type CustomSuperTokenCreatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum CustomSuperTokenCreatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

/**
 * Event: An interface which is shared by all
 * event entities and contains basic transaction
 * data.
 */
export type Event = {
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type Event_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum Event_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

/**
 * FlowUpdated: An `Event` entity that is emitted
 * when a flow is created, updated, or deleted.
 */
export type FlowUpdatedEvent = Event & {
  __typename?: 'FlowUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  /** The flow rate per second. */
  flowRate: Scalars['BigInt'];
  id: Scalars['ID'];
  /** The previous flow rate. */
  oldFlowRate: Scalars['BigInt'];
  receiver: Scalars['Bytes'];
  sender: Scalars['Bytes'];
  /** The stream entity which is being modified. */
  stream: Stream;
  timestamp: Scalars['BigInt'];
  /** The address of the `token` being streamed. */
  token: Scalars['Bytes'];
  /**
   * The total amount streamed until the timestamp
   * for the Stream entity linked to this event.
   */
  totalAmountStreamedUntilTimestamp: Scalars['BigInt'];
  totalReceiverFlowRate: Scalars['BigInt'];
  totalSenderFlowRate: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
  /**
   * The "type" of the `FlowUpdated` event.
   * 0 = create
   * 1 = update
   * 2 = terminate
   */
  type: Scalars['Int'];
  userData: Scalars['Bytes'];
};

export type FlowUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  flowRate?: Maybe<Scalars['BigInt']>;
  flowRate_gt?: Maybe<Scalars['BigInt']>;
  flowRate_gte?: Maybe<Scalars['BigInt']>;
  flowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  flowRate_lt?: Maybe<Scalars['BigInt']>;
  flowRate_lte?: Maybe<Scalars['BigInt']>;
  flowRate_not?: Maybe<Scalars['BigInt']>;
  flowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  oldFlowRate?: Maybe<Scalars['BigInt']>;
  oldFlowRate_gt?: Maybe<Scalars['BigInt']>;
  oldFlowRate_gte?: Maybe<Scalars['BigInt']>;
  oldFlowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  oldFlowRate_lt?: Maybe<Scalars['BigInt']>;
  oldFlowRate_lte?: Maybe<Scalars['BigInt']>;
  oldFlowRate_not?: Maybe<Scalars['BigInt']>;
  oldFlowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  receiver?: Maybe<Scalars['Bytes']>;
  receiver_contains?: Maybe<Scalars['Bytes']>;
  receiver_in?: Maybe<Array<Scalars['Bytes']>>;
  receiver_not?: Maybe<Scalars['Bytes']>;
  receiver_not_contains?: Maybe<Scalars['Bytes']>;
  receiver_not_in?: Maybe<Array<Scalars['Bytes']>>;
  sender?: Maybe<Scalars['Bytes']>;
  sender_contains?: Maybe<Scalars['Bytes']>;
  sender_in?: Maybe<Array<Scalars['Bytes']>>;
  sender_not?: Maybe<Scalars['Bytes']>;
  sender_not_contains?: Maybe<Scalars['Bytes']>;
  sender_not_in?: Maybe<Array<Scalars['Bytes']>>;
  stream?: Maybe<Scalars['String']>;
  stream_contains?: Maybe<Scalars['String']>;
  stream_ends_with?: Maybe<Scalars['String']>;
  stream_gt?: Maybe<Scalars['String']>;
  stream_gte?: Maybe<Scalars['String']>;
  stream_in?: Maybe<Array<Scalars['String']>>;
  stream_lt?: Maybe<Scalars['String']>;
  stream_lte?: Maybe<Scalars['String']>;
  stream_not?: Maybe<Scalars['String']>;
  stream_not_contains?: Maybe<Scalars['String']>;
  stream_not_ends_with?: Maybe<Scalars['String']>;
  stream_not_in?: Maybe<Array<Scalars['String']>>;
  stream_not_starts_with?: Maybe<Scalars['String']>;
  stream_starts_with?: Maybe<Scalars['String']>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  totalAmountStreamedUntilTimestamp?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_gt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_gte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountStreamedUntilTimestamp_lt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_lte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_not?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalReceiverFlowRate?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_gt?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_gte?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalReceiverFlowRate_lt?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_lte?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_not?: Maybe<Scalars['BigInt']>;
  totalReceiverFlowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSenderFlowRate?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_gt?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_gte?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSenderFlowRate_lt?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_lte?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_not?: Maybe<Scalars['BigInt']>;
  totalSenderFlowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  type?: Maybe<Scalars['Int']>;
  type_gt?: Maybe<Scalars['Int']>;
  type_gte?: Maybe<Scalars['Int']>;
  type_in?: Maybe<Array<Scalars['Int']>>;
  type_lt?: Maybe<Scalars['Int']>;
  type_lte?: Maybe<Scalars['Int']>;
  type_not?: Maybe<Scalars['Int']>;
  type_not_in?: Maybe<Array<Scalars['Int']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum FlowUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  FlowRate = 'flowRate',
  Id = 'id',
  OldFlowRate = 'oldFlowRate',
  Receiver = 'receiver',
  Sender = 'sender',
  Stream = 'stream',
  Timestamp = 'timestamp',
  Token = 'token',
  TotalAmountStreamedUntilTimestamp = 'totalAmountStreamedUntilTimestamp',
  TotalReceiverFlowRate = 'totalReceiverFlowRate',
  TotalSenderFlowRate = 'totalSenderFlowRate',
  TransactionHash = 'transactionHash',
  Type = 'type',
  UserData = 'userData'
}

export type GovernanceReplacedEvent = Event & {
  __typename?: 'GovernanceReplacedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  newGovernance: Scalars['Bytes'];
  oldGovernance: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type GovernanceReplacedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  newGovernance?: Maybe<Scalars['Bytes']>;
  newGovernance_contains?: Maybe<Scalars['Bytes']>;
  newGovernance_in?: Maybe<Array<Scalars['Bytes']>>;
  newGovernance_not?: Maybe<Scalars['Bytes']>;
  newGovernance_not_contains?: Maybe<Scalars['Bytes']>;
  newGovernance_not_in?: Maybe<Array<Scalars['Bytes']>>;
  oldGovernance?: Maybe<Scalars['Bytes']>;
  oldGovernance_contains?: Maybe<Scalars['Bytes']>;
  oldGovernance_in?: Maybe<Array<Scalars['Bytes']>>;
  oldGovernance_not?: Maybe<Scalars['Bytes']>;
  oldGovernance_not_contains?: Maybe<Scalars['Bytes']>;
  oldGovernance_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum GovernanceReplacedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  NewGovernance = 'newGovernance',
  OldGovernance = 'oldGovernance',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

/** Index: A Index HOL entity. */
export type Index = {
  __typename?: 'Index';
  createdAtBlockNumber: Scalars['BigInt'];
  createdAtTimestamp: Scalars['BigInt'];
  /** ID composed of: publisherAddress-tokenAddress-indexId */
  id: Scalars['ID'];
  /** IndexCreated event, there will only be one. */
  indexCreatedEvent: IndexCreatedEvent;
  indexDistributionClaimedEvents: Array<IndexDistributionClaimedEvent>;
  /** indexId is not the id of the `Index` entity. */
  indexId: Scalars['BigInt'];
  indexSubscribedEvents: Array<IndexSubscribedEvent>;
  indexUnitsUpdatedEvents: Array<IndexUnitsUpdatedEvent>;
  indexUnsubscribedEvents: Array<IndexUnsubscribedEvent>;
  indexUpdatedEvents: Array<IndexUpdatedEvent>;
  indexValue: Scalars['BigInt'];
  publisher: Account;
  /**
   * The subscriptions of the index, it will include approved, unapproved
   * and deleted subscriptions.
   */
  subscriptions: Array<IndexSubscription>;
  token: Token;
  /** The total amount distributed from this `Index`. */
  totalAmountDistributedUntilUpdatedAt: Scalars['BigInt'];
  /** The number of subscriptions which have units allocated to them on the `Index`. */
  totalSubscriptionsWithUnits: Scalars['Int'];
  /** The sum of `totalUnitsPending` and `totalUnitsApproved`. */
  totalUnits: Scalars['BigInt'];
  /**
   * The number of units allocated by the `Index` that are approved.
   * This refers to the current (as of updatedAt) `totalUnitsApproved`
   * - not all that has ever been approved.
   */
  totalUnitsApproved: Scalars['BigInt'];
  /**
   * The number of units allocated by the `Index` that are pending.
   * This refers to the current (as of updatedAt) `totalUnitsPending`
   * - not all that has ever been pending.
   */
  totalUnitsPending: Scalars['BigInt'];
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};


/** Index: A Index HOL entity. */
export type IndexIndexDistributionClaimedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexDistributionClaimedEvent_Filter>;
};


/** Index: A Index HOL entity. */
export type IndexIndexSubscribedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexSubscribedEvent_Filter>;
};


/** Index: A Index HOL entity. */
export type IndexIndexUnitsUpdatedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexUnitsUpdatedEvent_Filter>;
};


/** Index: A Index HOL entity. */
export type IndexIndexUnsubscribedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnsubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexUnsubscribedEvent_Filter>;
};


/** Index: A Index HOL entity. */
export type IndexIndexUpdatedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexUpdatedEvent_Filter>;
};


/** Index: A Index HOL entity. */
export type IndexSubscriptionsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscription_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<IndexSubscription_Filter>;
};

export type IndexCreatedEvent = Event & {
  __typename?: 'IndexCreatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type IndexCreatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexCreatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

export type IndexDistributionClaimedEvent = Event & {
  __typename?: 'IndexDistributionClaimedEvent';
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type IndexDistributionClaimedEvent_Filter = {
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexDistributionClaimedEvent_OrderBy {
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

export type IndexSubscribedEvent = Event & {
  __typename?: 'IndexSubscribedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type IndexSubscribedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexSubscribedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

/**
 * IndexSubscription: A HOL entity that contains subscription data for a `subscriber` account of a particular
 * `Index`.
 */
export type IndexSubscription = {
  __typename?: 'IndexSubscription';
  /**
   * Approved subscriptions don't require subscribers to claim tokens that are distributed from
   * the publisher.
   */
  approved: Scalars['Boolean'];
  createdAtBlockNumber: Scalars['BigInt'];
  createdAtTimestamp: Scalars['BigInt'];
  /** ID composed of: subscriberAddress-publisherAddress-tokenAddress-IndexId */
  id: Scalars['ID'];
  index: Index;
  /**
   * The previous index value - used to calculate `totalAmountReceivedUntilUpdatedAt` field as of the
   * `index.updatedAtTimestamp`. The formula to get this value is:
   * `IndexSubscription.totalAmountReceivedUntilUpdatedAt + ((index.newIndexValue - indexSubscription.indexValueUntilUpdatedAt) * indexSubscription.units)`.
   */
  indexValueUntilUpdatedAt: Scalars['BigInt'];
  subscriber: Account;
  /** IndexSubscription approved events on the subscription. */
  subscriptionApprovedEvents: Array<SubscriptionApprovedEvent>;
  subscriptionDistributionClaimedEvents: Array<SubscriptionDistributionClaimedEvent>;
  subscriptionRevokedEvents: Array<SubscriptionRevokedEvent>;
  subscriptionUnitsUpdatedEvents: Array<SubscriptionUnitsUpdatedEvent>;
  /**
   * The total amount of tokens you've received via IDA until
   * `updatedAtTimestamp`/`updatedAtBlock`.
   */
  totalAmountReceivedUntilUpdatedAt: Scalars['BigInt'];
  /**
   * If units is 0, it indicates that the subscription is "deleted". They are no longer
   * subscribed to the index.
   */
  units: Scalars['BigInt'];
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};


/**
 * IndexSubscription: A HOL entity that contains subscription data for a `subscriber` account of a particular
 * `Index`.
 */
export type IndexSubscriptionSubscriptionApprovedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionApprovedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<SubscriptionApprovedEvent_Filter>;
};


/**
 * IndexSubscription: A HOL entity that contains subscription data for a `subscriber` account of a particular
 * `Index`.
 */
export type IndexSubscriptionSubscriptionDistributionClaimedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<SubscriptionDistributionClaimedEvent_Filter>;
};


/**
 * IndexSubscription: A HOL entity that contains subscription data for a `subscriber` account of a particular
 * `Index`.
 */
export type IndexSubscriptionSubscriptionRevokedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionRevokedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<SubscriptionRevokedEvent_Filter>;
};


/**
 * IndexSubscription: A HOL entity that contains subscription data for a `subscriber` account of a particular
 * `Index`.
 */
export type IndexSubscriptionSubscriptionUnitsUpdatedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<SubscriptionUnitsUpdatedEvent_Filter>;
};

export type IndexSubscription_Filter = {
  approved?: Maybe<Scalars['Boolean']>;
  approved_in?: Maybe<Array<Scalars['Boolean']>>;
  approved_not?: Maybe<Scalars['Boolean']>;
  approved_not_in?: Maybe<Array<Scalars['Boolean']>>;
  createdAtBlockNumber?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexValueUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  indexValueUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  indexValueUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  subscriber?: Maybe<Scalars['String']>;
  subscriber_contains?: Maybe<Scalars['String']>;
  subscriber_ends_with?: Maybe<Scalars['String']>;
  subscriber_gt?: Maybe<Scalars['String']>;
  subscriber_gte?: Maybe<Scalars['String']>;
  subscriber_in?: Maybe<Array<Scalars['String']>>;
  subscriber_lt?: Maybe<Scalars['String']>;
  subscriber_lte?: Maybe<Scalars['String']>;
  subscriber_not?: Maybe<Scalars['String']>;
  subscriber_not_contains?: Maybe<Scalars['String']>;
  subscriber_not_ends_with?: Maybe<Scalars['String']>;
  subscriber_not_in?: Maybe<Array<Scalars['String']>>;
  subscriber_not_starts_with?: Maybe<Scalars['String']>;
  subscriber_starts_with?: Maybe<Scalars['String']>;
  totalAmountReceivedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountReceivedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountReceivedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  units?: Maybe<Scalars['BigInt']>;
  units_gt?: Maybe<Scalars['BigInt']>;
  units_gte?: Maybe<Scalars['BigInt']>;
  units_in?: Maybe<Array<Scalars['BigInt']>>;
  units_lt?: Maybe<Scalars['BigInt']>;
  units_lte?: Maybe<Scalars['BigInt']>;
  units_not?: Maybe<Scalars['BigInt']>;
  units_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum IndexSubscription_OrderBy {
  Approved = 'approved',
  CreatedAtBlockNumber = 'createdAtBlockNumber',
  CreatedAtTimestamp = 'createdAtTimestamp',
  Id = 'id',
  Index = 'index',
  IndexValueUntilUpdatedAt = 'indexValueUntilUpdatedAt',
  Subscriber = 'subscriber',
  SubscriptionApprovedEvents = 'subscriptionApprovedEvents',
  SubscriptionDistributionClaimedEvents = 'subscriptionDistributionClaimedEvents',
  SubscriptionRevokedEvents = 'subscriptionRevokedEvents',
  SubscriptionUnitsUpdatedEvents = 'subscriptionUnitsUpdatedEvents',
  TotalAmountReceivedUntilUpdatedAt = 'totalAmountReceivedUntilUpdatedAt',
  Units = 'units',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type IndexUnitsUpdatedEvent = Event & {
  __typename?: 'IndexUnitsUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  oldUnits: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  units: Scalars['BigInt'];
  userData: Scalars['Bytes'];
};

export type IndexUnitsUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  oldUnits?: Maybe<Scalars['BigInt']>;
  oldUnits_gt?: Maybe<Scalars['BigInt']>;
  oldUnits_gte?: Maybe<Scalars['BigInt']>;
  oldUnits_in?: Maybe<Array<Scalars['BigInt']>>;
  oldUnits_lt?: Maybe<Scalars['BigInt']>;
  oldUnits_lte?: Maybe<Scalars['BigInt']>;
  oldUnits_not?: Maybe<Scalars['BigInt']>;
  oldUnits_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  units?: Maybe<Scalars['BigInt']>;
  units_gt?: Maybe<Scalars['BigInt']>;
  units_gte?: Maybe<Scalars['BigInt']>;
  units_in?: Maybe<Array<Scalars['BigInt']>>;
  units_lt?: Maybe<Scalars['BigInt']>;
  units_lte?: Maybe<Scalars['BigInt']>;
  units_not?: Maybe<Scalars['BigInt']>;
  units_not_in?: Maybe<Array<Scalars['BigInt']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexUnitsUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  OldUnits = 'oldUnits',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  Units = 'units',
  UserData = 'userData'
}

export type IndexUnsubscribedEvent = Event & {
  __typename?: 'IndexUnsubscribedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type IndexUnsubscribedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexUnsubscribedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

export type IndexUpdatedEvent = Event & {
  __typename?: 'IndexUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  index: Index;
  indexId: Scalars['BigInt'];
  newIndexValue: Scalars['BigInt'];
  oldIndexValue: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  totalUnitsApproved: Scalars['BigInt'];
  totalUnitsPending: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type IndexUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  index?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  index_contains?: Maybe<Scalars['String']>;
  index_ends_with?: Maybe<Scalars['String']>;
  index_gt?: Maybe<Scalars['String']>;
  index_gte?: Maybe<Scalars['String']>;
  index_in?: Maybe<Array<Scalars['String']>>;
  index_lt?: Maybe<Scalars['String']>;
  index_lte?: Maybe<Scalars['String']>;
  index_not?: Maybe<Scalars['String']>;
  index_not_contains?: Maybe<Scalars['String']>;
  index_not_ends_with?: Maybe<Scalars['String']>;
  index_not_in?: Maybe<Array<Scalars['String']>>;
  index_not_starts_with?: Maybe<Scalars['String']>;
  index_starts_with?: Maybe<Scalars['String']>;
  newIndexValue?: Maybe<Scalars['BigInt']>;
  newIndexValue_gt?: Maybe<Scalars['BigInt']>;
  newIndexValue_gte?: Maybe<Scalars['BigInt']>;
  newIndexValue_in?: Maybe<Array<Scalars['BigInt']>>;
  newIndexValue_lt?: Maybe<Scalars['BigInt']>;
  newIndexValue_lte?: Maybe<Scalars['BigInt']>;
  newIndexValue_not?: Maybe<Scalars['BigInt']>;
  newIndexValue_not_in?: Maybe<Array<Scalars['BigInt']>>;
  oldIndexValue?: Maybe<Scalars['BigInt']>;
  oldIndexValue_gt?: Maybe<Scalars['BigInt']>;
  oldIndexValue_gte?: Maybe<Scalars['BigInt']>;
  oldIndexValue_in?: Maybe<Array<Scalars['BigInt']>>;
  oldIndexValue_lt?: Maybe<Scalars['BigInt']>;
  oldIndexValue_lte?: Maybe<Scalars['BigInt']>;
  oldIndexValue_not?: Maybe<Scalars['BigInt']>;
  oldIndexValue_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  totalUnitsApproved?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_gt?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_gte?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsApproved_lt?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_lte?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_not?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsPending?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_gt?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_gte?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsPending_lt?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_lte?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_not?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum IndexUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Index = 'index',
  IndexId = 'indexId',
  NewIndexValue = 'newIndexValue',
  OldIndexValue = 'oldIndexValue',
  Publisher = 'publisher',
  Timestamp = 'timestamp',
  Token = 'token',
  TotalUnitsApproved = 'totalUnitsApproved',
  TotalUnitsPending = 'totalUnitsPending',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

export type Index_Filter = {
  createdAtBlockNumber?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  indexCreatedEvent?: Maybe<Scalars['String']>;
  indexCreatedEvent_contains?: Maybe<Scalars['String']>;
  indexCreatedEvent_ends_with?: Maybe<Scalars['String']>;
  indexCreatedEvent_gt?: Maybe<Scalars['String']>;
  indexCreatedEvent_gte?: Maybe<Scalars['String']>;
  indexCreatedEvent_in?: Maybe<Array<Scalars['String']>>;
  indexCreatedEvent_lt?: Maybe<Scalars['String']>;
  indexCreatedEvent_lte?: Maybe<Scalars['String']>;
  indexCreatedEvent_not?: Maybe<Scalars['String']>;
  indexCreatedEvent_not_contains?: Maybe<Scalars['String']>;
  indexCreatedEvent_not_ends_with?: Maybe<Scalars['String']>;
  indexCreatedEvent_not_in?: Maybe<Array<Scalars['String']>>;
  indexCreatedEvent_not_starts_with?: Maybe<Scalars['String']>;
  indexCreatedEvent_starts_with?: Maybe<Scalars['String']>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  indexValue?: Maybe<Scalars['BigInt']>;
  indexValue_gt?: Maybe<Scalars['BigInt']>;
  indexValue_gte?: Maybe<Scalars['BigInt']>;
  indexValue_in?: Maybe<Array<Scalars['BigInt']>>;
  indexValue_lt?: Maybe<Scalars['BigInt']>;
  indexValue_lte?: Maybe<Scalars['BigInt']>;
  indexValue_not?: Maybe<Scalars['BigInt']>;
  indexValue_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['String']>;
  publisher_contains?: Maybe<Scalars['String']>;
  publisher_ends_with?: Maybe<Scalars['String']>;
  publisher_gt?: Maybe<Scalars['String']>;
  publisher_gte?: Maybe<Scalars['String']>;
  publisher_in?: Maybe<Array<Scalars['String']>>;
  publisher_lt?: Maybe<Scalars['String']>;
  publisher_lte?: Maybe<Scalars['String']>;
  publisher_not?: Maybe<Scalars['String']>;
  publisher_not_contains?: Maybe<Scalars['String']>;
  publisher_not_ends_with?: Maybe<Scalars['String']>;
  publisher_not_in?: Maybe<Array<Scalars['String']>>;
  publisher_not_starts_with?: Maybe<Scalars['String']>;
  publisher_starts_with?: Maybe<Scalars['String']>;
  token?: Maybe<Scalars['String']>;
  token_contains?: Maybe<Scalars['String']>;
  token_ends_with?: Maybe<Scalars['String']>;
  token_gt?: Maybe<Scalars['String']>;
  token_gte?: Maybe<Scalars['String']>;
  token_in?: Maybe<Array<Scalars['String']>>;
  token_lt?: Maybe<Scalars['String']>;
  token_lte?: Maybe<Scalars['String']>;
  token_not?: Maybe<Scalars['String']>;
  token_not_contains?: Maybe<Scalars['String']>;
  token_not_ends_with?: Maybe<Scalars['String']>;
  token_not_in?: Maybe<Array<Scalars['String']>>;
  token_not_starts_with?: Maybe<Scalars['String']>;
  token_starts_with?: Maybe<Scalars['String']>;
  totalAmountDistributedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountDistributedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSubscriptionsWithUnits?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_in?: Maybe<Array<Scalars['Int']>>;
  totalSubscriptionsWithUnits_lt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_lte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not_in?: Maybe<Array<Scalars['Int']>>;
  totalUnits?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_gt?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_gte?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsApproved_lt?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_lte?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_not?: Maybe<Scalars['BigInt']>;
  totalUnitsApproved_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsPending?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_gt?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_gte?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnitsPending_lt?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_lte?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_not?: Maybe<Scalars['BigInt']>;
  totalUnitsPending_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnits_gt?: Maybe<Scalars['BigInt']>;
  totalUnits_gte?: Maybe<Scalars['BigInt']>;
  totalUnits_in?: Maybe<Array<Scalars['BigInt']>>;
  totalUnits_lt?: Maybe<Scalars['BigInt']>;
  totalUnits_lte?: Maybe<Scalars['BigInt']>;
  totalUnits_not?: Maybe<Scalars['BigInt']>;
  totalUnits_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum Index_OrderBy {
  CreatedAtBlockNumber = 'createdAtBlockNumber',
  CreatedAtTimestamp = 'createdAtTimestamp',
  Id = 'id',
  IndexCreatedEvent = 'indexCreatedEvent',
  IndexDistributionClaimedEvents = 'indexDistributionClaimedEvents',
  IndexId = 'indexId',
  IndexSubscribedEvents = 'indexSubscribedEvents',
  IndexUnitsUpdatedEvents = 'indexUnitsUpdatedEvents',
  IndexUnsubscribedEvents = 'indexUnsubscribedEvents',
  IndexUpdatedEvents = 'indexUpdatedEvents',
  IndexValue = 'indexValue',
  Publisher = 'publisher',
  Subscriptions = 'subscriptions',
  Token = 'token',
  TotalAmountDistributedUntilUpdatedAt = 'totalAmountDistributedUntilUpdatedAt',
  TotalSubscriptionsWithUnits = 'totalSubscriptionsWithUnits',
  TotalUnits = 'totalUnits',
  TotalUnitsApproved = 'totalUnitsApproved',
  TotalUnitsPending = 'totalUnitsPending',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type JailEvent = Event & {
  __typename?: 'JailEvent';
  app: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  reason: Scalars['BigInt'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type JailEvent_Filter = {
  app?: Maybe<Scalars['Bytes']>;
  app_contains?: Maybe<Scalars['Bytes']>;
  app_in?: Maybe<Array<Scalars['Bytes']>>;
  app_not?: Maybe<Scalars['Bytes']>;
  app_not_contains?: Maybe<Scalars['Bytes']>;
  app_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  reason?: Maybe<Scalars['BigInt']>;
  reason_gt?: Maybe<Scalars['BigInt']>;
  reason_gte?: Maybe<Scalars['BigInt']>;
  reason_in?: Maybe<Array<Scalars['BigInt']>>;
  reason_lt?: Maybe<Scalars['BigInt']>;
  reason_lte?: Maybe<Scalars['BigInt']>;
  reason_not?: Maybe<Scalars['BigInt']>;
  reason_not_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum JailEvent_OrderBy {
  App = 'app',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Reason = 'reason',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type MintedEvent = Event & {
  __typename?: 'MintedEvent';
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  data: Scalars['Bytes'];
  id: Scalars['ID'];
  operator: Scalars['Bytes'];
  operatorData: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  to: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type MintedEvent_Filter = {
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  data?: Maybe<Scalars['Bytes']>;
  data_contains?: Maybe<Scalars['Bytes']>;
  data_in?: Maybe<Array<Scalars['Bytes']>>;
  data_not?: Maybe<Scalars['Bytes']>;
  data_not_contains?: Maybe<Scalars['Bytes']>;
  data_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  operator?: Maybe<Scalars['Bytes']>;
  operatorData?: Maybe<Scalars['Bytes']>;
  operatorData_contains?: Maybe<Scalars['Bytes']>;
  operatorData_in?: Maybe<Array<Scalars['Bytes']>>;
  operatorData_not?: Maybe<Scalars['Bytes']>;
  operatorData_not_contains?: Maybe<Scalars['Bytes']>;
  operatorData_not_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_contains?: Maybe<Scalars['Bytes']>;
  operator_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_not?: Maybe<Scalars['Bytes']>;
  operator_not_contains?: Maybe<Scalars['Bytes']>;
  operator_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  to?: Maybe<Scalars['Bytes']>;
  to_contains?: Maybe<Scalars['Bytes']>;
  to_in?: Maybe<Array<Scalars['Bytes']>>;
  to_not?: Maybe<Scalars['Bytes']>;
  to_not_contains?: Maybe<Scalars['Bytes']>;
  to_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum MintedEvent_OrderBy {
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Data = 'data',
  Id = 'id',
  Operator = 'operator',
  OperatorData = 'operatorData',
  Timestamp = 'timestamp',
  To = 'to',
  TransactionHash = 'transactionHash'
}

export enum OrderDirection {
  Asc = 'asc',
  Desc = 'desc'
}

export type Query = {
  __typename?: 'Query';
  /** Access to subgraph metadata */
  _meta?: Maybe<_Meta_>;
  account?: Maybe<Account>;
  accountTokenSnapshot?: Maybe<AccountTokenSnapshot>;
  accountTokenSnapshots: Array<AccountTokenSnapshot>;
  accounts: Array<Account>;
  agreementClassRegisteredEvent?: Maybe<AgreementClassRegisteredEvent>;
  agreementClassRegisteredEvents: Array<AgreementClassRegisteredEvent>;
  agreementClassUpdatedEvent?: Maybe<AgreementClassUpdatedEvent>;
  agreementClassUpdatedEvents: Array<AgreementClassUpdatedEvent>;
  agreementLiquidatedByEvent?: Maybe<AgreementLiquidatedByEvent>;
  agreementLiquidatedByEvents: Array<AgreementLiquidatedByEvent>;
  appRegisteredEvent?: Maybe<AppRegisteredEvent>;
  appRegisteredEvents: Array<AppRegisteredEvent>;
  burnedEvent?: Maybe<BurnedEvent>;
  burnedEvents: Array<BurnedEvent>;
  cfav1LiquidationPeriodChangedEvent?: Maybe<CfAv1LiquidationPeriodChangedEvent>;
  cfav1LiquidationPeriodChangedEvents: Array<CfAv1LiquidationPeriodChangedEvent>;
  configChangedEvent?: Maybe<ConfigChangedEvent>;
  configChangedEvents: Array<ConfigChangedEvent>;
  customSuperTokenCreatedEvent?: Maybe<CustomSuperTokenCreatedEvent>;
  customSuperTokenCreatedEvents: Array<CustomSuperTokenCreatedEvent>;
  event?: Maybe<Event>;
  events: Array<Event>;
  flowUpdatedEvent?: Maybe<FlowUpdatedEvent>;
  flowUpdatedEvents: Array<FlowUpdatedEvent>;
  governanceReplacedEvent?: Maybe<GovernanceReplacedEvent>;
  governanceReplacedEvents: Array<GovernanceReplacedEvent>;
  index?: Maybe<Index>;
  indexCreatedEvent?: Maybe<IndexCreatedEvent>;
  indexCreatedEvents: Array<IndexCreatedEvent>;
  indexDistributionClaimedEvent?: Maybe<IndexDistributionClaimedEvent>;
  indexDistributionClaimedEvents: Array<IndexDistributionClaimedEvent>;
  indexSubscribedEvent?: Maybe<IndexSubscribedEvent>;
  indexSubscribedEvents: Array<IndexSubscribedEvent>;
  indexSubscription?: Maybe<IndexSubscription>;
  indexSubscriptions: Array<IndexSubscription>;
  indexUnitsUpdatedEvent?: Maybe<IndexUnitsUpdatedEvent>;
  indexUnitsUpdatedEvents: Array<IndexUnitsUpdatedEvent>;
  indexUnsubscribedEvent?: Maybe<IndexUnsubscribedEvent>;
  indexUnsubscribedEvents: Array<IndexUnsubscribedEvent>;
  indexUpdatedEvent?: Maybe<IndexUpdatedEvent>;
  indexUpdatedEvents: Array<IndexUpdatedEvent>;
  indexes: Array<Index>;
  jailEvent?: Maybe<JailEvent>;
  jailEvents: Array<JailEvent>;
  mintedEvent?: Maybe<MintedEvent>;
  mintedEvents: Array<MintedEvent>;
  rewardAddressChangedEvent?: Maybe<RewardAddressChangedEvent>;
  rewardAddressChangedEvents: Array<RewardAddressChangedEvent>;
  roleAdminChangedEvent?: Maybe<RoleAdminChangedEvent>;
  roleAdminChangedEvents: Array<RoleAdminChangedEvent>;
  roleGrantedEvent?: Maybe<RoleGrantedEvent>;
  roleGrantedEvents: Array<RoleGrantedEvent>;
  roleRevokedEvent?: Maybe<RoleRevokedEvent>;
  roleRevokedEvents: Array<RoleRevokedEvent>;
  sentEvent?: Maybe<SentEvent>;
  sentEvents: Array<SentEvent>;
  stream?: Maybe<Stream>;
  streamRevision?: Maybe<StreamRevision>;
  streamRevisions: Array<StreamRevision>;
  streams: Array<Stream>;
  subscriptionApprovedEvent?: Maybe<SubscriptionApprovedEvent>;
  subscriptionApprovedEvents: Array<SubscriptionApprovedEvent>;
  subscriptionDistributionClaimedEvent?: Maybe<SubscriptionDistributionClaimedEvent>;
  subscriptionDistributionClaimedEvents: Array<SubscriptionDistributionClaimedEvent>;
  subscriptionRevokedEvent?: Maybe<SubscriptionRevokedEvent>;
  subscriptionRevokedEvents: Array<SubscriptionRevokedEvent>;
  subscriptionUnitsUpdatedEvent?: Maybe<SubscriptionUnitsUpdatedEvent>;
  subscriptionUnitsUpdatedEvents: Array<SubscriptionUnitsUpdatedEvent>;
  superTokenCreatedEvent?: Maybe<SuperTokenCreatedEvent>;
  superTokenCreatedEvents: Array<SuperTokenCreatedEvent>;
  superTokenFactoryUpdatedEvent?: Maybe<SuperTokenFactoryUpdatedEvent>;
  superTokenFactoryUpdatedEvents: Array<SuperTokenFactoryUpdatedEvent>;
  superTokenLogicCreatedEvent?: Maybe<SuperTokenLogicCreatedEvent>;
  superTokenLogicCreatedEvents: Array<SuperTokenLogicCreatedEvent>;
  superTokenLogicUpdatedEvent?: Maybe<SuperTokenLogicUpdatedEvent>;
  superTokenLogicUpdatedEvents: Array<SuperTokenLogicUpdatedEvent>;
  token?: Maybe<Token>;
  tokenDowngradedEvent?: Maybe<TokenDowngradedEvent>;
  tokenDowngradedEvents: Array<TokenDowngradedEvent>;
  tokenStatistic?: Maybe<TokenStatistic>;
  tokenStatistics: Array<TokenStatistic>;
  tokenUpgradedEvent?: Maybe<TokenUpgradedEvent>;
  tokenUpgradedEvents: Array<TokenUpgradedEvent>;
  tokens: Array<Token>;
  transferEvent?: Maybe<TransferEvent>;
  transferEvents: Array<TransferEvent>;
  trustedForwarderChangedEvent?: Maybe<TrustedForwarderChangedEvent>;
  trustedForwarderChangedEvents: Array<TrustedForwarderChangedEvent>;
};


export type Query_MetaArgs = {
  block?: Maybe<Block_Height>;
};


export type QueryAccountArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAccountTokenSnapshotArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAccountTokenSnapshotsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AccountTokenSnapshot_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AccountTokenSnapshot_Filter>;
};


export type QueryAccountsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Account_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Account_Filter>;
};


export type QueryAgreementClassRegisteredEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAgreementClassRegisteredEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementClassRegisteredEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementClassRegisteredEvent_Filter>;
};


export type QueryAgreementClassUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAgreementClassUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementClassUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementClassUpdatedEvent_Filter>;
};


export type QueryAgreementLiquidatedByEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAgreementLiquidatedByEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementLiquidatedByEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementLiquidatedByEvent_Filter>;
};


export type QueryAppRegisteredEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryAppRegisteredEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AppRegisteredEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AppRegisteredEvent_Filter>;
};


export type QueryBurnedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryBurnedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<BurnedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<BurnedEvent_Filter>;
};


export type QueryCfav1LiquidationPeriodChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryCfav1LiquidationPeriodChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<CfAv1LiquidationPeriodChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<CfAv1LiquidationPeriodChangedEvent_Filter>;
};


export type QueryConfigChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryConfigChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<ConfigChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<ConfigChangedEvent_Filter>;
};


export type QueryCustomSuperTokenCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryCustomSuperTokenCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<CustomSuperTokenCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<CustomSuperTokenCreatedEvent_Filter>;
};


export type QueryEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Event_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Event_Filter>;
};


export type QueryFlowUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryFlowUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<FlowUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<FlowUpdatedEvent_Filter>;
};


export type QueryGovernanceReplacedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryGovernanceReplacedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<GovernanceReplacedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<GovernanceReplacedEvent_Filter>;
};


export type QueryIndexArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexCreatedEvent_Filter>;
};


export type QueryIndexDistributionClaimedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexDistributionClaimedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexDistributionClaimedEvent_Filter>;
};


export type QueryIndexSubscribedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexSubscribedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexSubscribedEvent_Filter>;
};


export type QueryIndexSubscriptionArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexSubscriptionsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscription_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexSubscription_Filter>;
};


export type QueryIndexUnitsUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexUnitsUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUnitsUpdatedEvent_Filter>;
};


export type QueryIndexUnsubscribedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexUnsubscribedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnsubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUnsubscribedEvent_Filter>;
};


export type QueryIndexUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryIndexUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUpdatedEvent_Filter>;
};


export type QueryIndexesArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Index_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Index_Filter>;
};


export type QueryJailEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryJailEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<JailEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<JailEvent_Filter>;
};


export type QueryMintedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryMintedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<MintedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<MintedEvent_Filter>;
};


export type QueryRewardAddressChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryRewardAddressChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RewardAddressChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RewardAddressChangedEvent_Filter>;
};


export type QueryRoleAdminChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryRoleAdminChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleAdminChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleAdminChangedEvent_Filter>;
};


export type QueryRoleGrantedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryRoleGrantedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleGrantedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleGrantedEvent_Filter>;
};


export type QueryRoleRevokedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryRoleRevokedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleRevokedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleRevokedEvent_Filter>;
};


export type QuerySentEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySentEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SentEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SentEvent_Filter>;
};


export type QueryStreamArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryStreamRevisionArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryStreamRevisionsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<StreamRevision_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<StreamRevision_Filter>;
};


export type QueryStreamsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Stream_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Stream_Filter>;
};


export type QuerySubscriptionApprovedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySubscriptionApprovedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionApprovedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionApprovedEvent_Filter>;
};


export type QuerySubscriptionDistributionClaimedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySubscriptionDistributionClaimedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionDistributionClaimedEvent_Filter>;
};


export type QuerySubscriptionRevokedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySubscriptionRevokedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionRevokedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionRevokedEvent_Filter>;
};


export type QuerySubscriptionUnitsUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySubscriptionUnitsUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionUnitsUpdatedEvent_Filter>;
};


export type QuerySuperTokenCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySuperTokenCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenCreatedEvent_Filter>;
};


export type QuerySuperTokenFactoryUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySuperTokenFactoryUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenFactoryUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenFactoryUpdatedEvent_Filter>;
};


export type QuerySuperTokenLogicCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySuperTokenLogicCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenLogicCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenLogicCreatedEvent_Filter>;
};


export type QuerySuperTokenLogicUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QuerySuperTokenLogicUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenLogicUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenLogicUpdatedEvent_Filter>;
};


export type QueryTokenArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTokenDowngradedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTokenDowngradedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenDowngradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenDowngradedEvent_Filter>;
};


export type QueryTokenStatisticArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTokenStatisticsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenStatistic_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenStatistic_Filter>;
};


export type QueryTokenUpgradedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTokenUpgradedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenUpgradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenUpgradedEvent_Filter>;
};


export type QueryTokensArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Token_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Token_Filter>;
};


export type QueryTransferEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTransferEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TransferEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TransferEvent_Filter>;
};


export type QueryTrustedForwarderChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type QueryTrustedForwarderChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TrustedForwarderChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TrustedForwarderChangedEvent_Filter>;
};

export type RewardAddressChangedEvent = Event & {
  __typename?: 'RewardAddressChangedEvent';
  blockNumber: Scalars['BigInt'];
  host: Scalars['Bytes'];
  id: Scalars['ID'];
  isSet: Scalars['Boolean'];
  rewardAddress: Scalars['Bytes'];
  superToken: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type RewardAddressChangedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  host?: Maybe<Scalars['Bytes']>;
  host_contains?: Maybe<Scalars['Bytes']>;
  host_in?: Maybe<Array<Scalars['Bytes']>>;
  host_not?: Maybe<Scalars['Bytes']>;
  host_not_contains?: Maybe<Scalars['Bytes']>;
  host_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isSet?: Maybe<Scalars['Boolean']>;
  isSet_in?: Maybe<Array<Scalars['Boolean']>>;
  isSet_not?: Maybe<Scalars['Boolean']>;
  isSet_not_in?: Maybe<Array<Scalars['Boolean']>>;
  rewardAddress?: Maybe<Scalars['Bytes']>;
  rewardAddress_contains?: Maybe<Scalars['Bytes']>;
  rewardAddress_in?: Maybe<Array<Scalars['Bytes']>>;
  rewardAddress_not?: Maybe<Scalars['Bytes']>;
  rewardAddress_not_contains?: Maybe<Scalars['Bytes']>;
  rewardAddress_not_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken?: Maybe<Scalars['Bytes']>;
  superToken_contains?: Maybe<Scalars['Bytes']>;
  superToken_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken_not?: Maybe<Scalars['Bytes']>;
  superToken_not_contains?: Maybe<Scalars['Bytes']>;
  superToken_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum RewardAddressChangedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Host = 'host',
  Id = 'id',
  IsSet = 'isSet',
  RewardAddress = 'rewardAddress',
  SuperToken = 'superToken',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type RoleAdminChangedEvent = Event & {
  __typename?: 'RoleAdminChangedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  newAdminRole: Scalars['Bytes'];
  previousAdminRole: Scalars['Bytes'];
  role: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type RoleAdminChangedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  newAdminRole?: Maybe<Scalars['Bytes']>;
  newAdminRole_contains?: Maybe<Scalars['Bytes']>;
  newAdminRole_in?: Maybe<Array<Scalars['Bytes']>>;
  newAdminRole_not?: Maybe<Scalars['Bytes']>;
  newAdminRole_not_contains?: Maybe<Scalars['Bytes']>;
  newAdminRole_not_in?: Maybe<Array<Scalars['Bytes']>>;
  previousAdminRole?: Maybe<Scalars['Bytes']>;
  previousAdminRole_contains?: Maybe<Scalars['Bytes']>;
  previousAdminRole_in?: Maybe<Array<Scalars['Bytes']>>;
  previousAdminRole_not?: Maybe<Scalars['Bytes']>;
  previousAdminRole_not_contains?: Maybe<Scalars['Bytes']>;
  previousAdminRole_not_in?: Maybe<Array<Scalars['Bytes']>>;
  role?: Maybe<Scalars['Bytes']>;
  role_contains?: Maybe<Scalars['Bytes']>;
  role_in?: Maybe<Array<Scalars['Bytes']>>;
  role_not?: Maybe<Scalars['Bytes']>;
  role_not_contains?: Maybe<Scalars['Bytes']>;
  role_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum RoleAdminChangedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  NewAdminRole = 'newAdminRole',
  PreviousAdminRole = 'previousAdminRole',
  Role = 'role',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type RoleGrantedEvent = Event & {
  __typename?: 'RoleGrantedEvent';
  account: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  role: Scalars['Bytes'];
  sender: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type RoleGrantedEvent_Filter = {
  account?: Maybe<Scalars['Bytes']>;
  account_contains?: Maybe<Scalars['Bytes']>;
  account_in?: Maybe<Array<Scalars['Bytes']>>;
  account_not?: Maybe<Scalars['Bytes']>;
  account_not_contains?: Maybe<Scalars['Bytes']>;
  account_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  role?: Maybe<Scalars['Bytes']>;
  role_contains?: Maybe<Scalars['Bytes']>;
  role_in?: Maybe<Array<Scalars['Bytes']>>;
  role_not?: Maybe<Scalars['Bytes']>;
  role_not_contains?: Maybe<Scalars['Bytes']>;
  role_not_in?: Maybe<Array<Scalars['Bytes']>>;
  sender?: Maybe<Scalars['Bytes']>;
  sender_contains?: Maybe<Scalars['Bytes']>;
  sender_in?: Maybe<Array<Scalars['Bytes']>>;
  sender_not?: Maybe<Scalars['Bytes']>;
  sender_not_contains?: Maybe<Scalars['Bytes']>;
  sender_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum RoleGrantedEvent_OrderBy {
  Account = 'account',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Role = 'role',
  Sender = 'sender',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type RoleRevokedEvent = Event & {
  __typename?: 'RoleRevokedEvent';
  account: Scalars['Bytes'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  role: Scalars['Bytes'];
  sender: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type RoleRevokedEvent_Filter = {
  account?: Maybe<Scalars['Bytes']>;
  account_contains?: Maybe<Scalars['Bytes']>;
  account_in?: Maybe<Array<Scalars['Bytes']>>;
  account_not?: Maybe<Scalars['Bytes']>;
  account_not_contains?: Maybe<Scalars['Bytes']>;
  account_not_in?: Maybe<Array<Scalars['Bytes']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  role?: Maybe<Scalars['Bytes']>;
  role_contains?: Maybe<Scalars['Bytes']>;
  role_in?: Maybe<Array<Scalars['Bytes']>>;
  role_not?: Maybe<Scalars['Bytes']>;
  role_not_contains?: Maybe<Scalars['Bytes']>;
  role_not_in?: Maybe<Array<Scalars['Bytes']>>;
  sender?: Maybe<Scalars['Bytes']>;
  sender_contains?: Maybe<Scalars['Bytes']>;
  sender_in?: Maybe<Array<Scalars['Bytes']>>;
  sender_not?: Maybe<Scalars['Bytes']>;
  sender_not_contains?: Maybe<Scalars['Bytes']>;
  sender_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum RoleRevokedEvent_OrderBy {
  Account = 'account',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Role = 'role',
  Sender = 'sender',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type SentEvent = Event & {
  __typename?: 'SentEvent';
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  data: Scalars['Bytes'];
  id: Scalars['ID'];
  operator: Scalars['Bytes'];
  operatorData: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  to: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type SentEvent_Filter = {
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  data?: Maybe<Scalars['Bytes']>;
  data_contains?: Maybe<Scalars['Bytes']>;
  data_in?: Maybe<Array<Scalars['Bytes']>>;
  data_not?: Maybe<Scalars['Bytes']>;
  data_not_contains?: Maybe<Scalars['Bytes']>;
  data_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  operator?: Maybe<Scalars['Bytes']>;
  operatorData?: Maybe<Scalars['Bytes']>;
  operatorData_contains?: Maybe<Scalars['Bytes']>;
  operatorData_in?: Maybe<Array<Scalars['Bytes']>>;
  operatorData_not?: Maybe<Scalars['Bytes']>;
  operatorData_not_contains?: Maybe<Scalars['Bytes']>;
  operatorData_not_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_contains?: Maybe<Scalars['Bytes']>;
  operator_in?: Maybe<Array<Scalars['Bytes']>>;
  operator_not?: Maybe<Scalars['Bytes']>;
  operator_not_contains?: Maybe<Scalars['Bytes']>;
  operator_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  to?: Maybe<Scalars['Bytes']>;
  to_contains?: Maybe<Scalars['Bytes']>;
  to_in?: Maybe<Array<Scalars['Bytes']>>;
  to_not?: Maybe<Scalars['Bytes']>;
  to_not_contains?: Maybe<Scalars['Bytes']>;
  to_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SentEvent_OrderBy {
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Data = 'data',
  Id = 'id',
  Operator = 'operator',
  OperatorData = 'operatorData',
  Timestamp = 'timestamp',
  To = 'to',
  TransactionHash = 'transactionHash'
}

/**
 * Stream: A HOL entity that represents the lifetime of a stream between a `sender` and a `receiver`.
 * A account can start a stream, update the flow rate, but when they close it, it is
 * considered "dead". The next stream you create with the same `sender` and `receiver`
 * will create a new stream entity. Therefore, multiple stream entities can be created
 * between the same `sender` and `receiver`.
 */
export type Stream = {
  __typename?: 'Stream';
  createdAtBlockNumber: Scalars['BigInt'];
  createdAtTimestamp: Scalars['BigInt'];
  currentFlowRate: Scalars['BigInt'];
  flowUpdatedEvents: Array<FlowUpdatedEvent>;
  /** ID composed of: senderAddress-receiverAddress-tokenAddress-revisionIndex */
  id: Scalars['ID'];
  receiver: Account;
  sender: Account;
  /**
   * The amount streamed until `updatedAtTimestamp`/`updatedAtBlock`. The formula to get the current streamed
   * amount is:
   * `streamedUntilUpdatedAt + ((currentTime in seconds) - updatedAtTimestamp) * currentFlowRate`.
   */
  streamedUntilUpdatedAt: Scalars['BigInt'];
  token: Token;
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};


/**
 * Stream: A HOL entity that represents the lifetime of a stream between a `sender` and a `receiver`.
 * A account can start a stream, update the flow rate, but when they close it, it is
 * considered "dead". The next stream you create with the same `sender` and `receiver`
 * will create a new stream entity. Therefore, multiple stream entities can be created
 * between the same `sender` and `receiver`.
 */
export type StreamFlowUpdatedEventsArgs = {
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<FlowUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  where?: Maybe<FlowUpdatedEvent_Filter>;
};

export type StreamRevision = {
  __typename?: 'StreamRevision';
  id: Scalars['ID'];
  revisionIndex: Scalars['Int'];
};

export type StreamRevision_Filter = {
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  revisionIndex?: Maybe<Scalars['Int']>;
  revisionIndex_gt?: Maybe<Scalars['Int']>;
  revisionIndex_gte?: Maybe<Scalars['Int']>;
  revisionIndex_in?: Maybe<Array<Scalars['Int']>>;
  revisionIndex_lt?: Maybe<Scalars['Int']>;
  revisionIndex_lte?: Maybe<Scalars['Int']>;
  revisionIndex_not?: Maybe<Scalars['Int']>;
  revisionIndex_not_in?: Maybe<Array<Scalars['Int']>>;
};

export enum StreamRevision_OrderBy {
  Id = 'id',
  RevisionIndex = 'revisionIndex'
}

export type Stream_Filter = {
  createdAtBlockNumber?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  currentFlowRate?: Maybe<Scalars['BigInt']>;
  currentFlowRate_gt?: Maybe<Scalars['BigInt']>;
  currentFlowRate_gte?: Maybe<Scalars['BigInt']>;
  currentFlowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  currentFlowRate_lt?: Maybe<Scalars['BigInt']>;
  currentFlowRate_lte?: Maybe<Scalars['BigInt']>;
  currentFlowRate_not?: Maybe<Scalars['BigInt']>;
  currentFlowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  receiver?: Maybe<Scalars['String']>;
  receiver_contains?: Maybe<Scalars['String']>;
  receiver_ends_with?: Maybe<Scalars['String']>;
  receiver_gt?: Maybe<Scalars['String']>;
  receiver_gte?: Maybe<Scalars['String']>;
  receiver_in?: Maybe<Array<Scalars['String']>>;
  receiver_lt?: Maybe<Scalars['String']>;
  receiver_lte?: Maybe<Scalars['String']>;
  receiver_not?: Maybe<Scalars['String']>;
  receiver_not_contains?: Maybe<Scalars['String']>;
  receiver_not_ends_with?: Maybe<Scalars['String']>;
  receiver_not_in?: Maybe<Array<Scalars['String']>>;
  receiver_not_starts_with?: Maybe<Scalars['String']>;
  receiver_starts_with?: Maybe<Scalars['String']>;
  sender?: Maybe<Scalars['String']>;
  sender_contains?: Maybe<Scalars['String']>;
  sender_ends_with?: Maybe<Scalars['String']>;
  sender_gt?: Maybe<Scalars['String']>;
  sender_gte?: Maybe<Scalars['String']>;
  sender_in?: Maybe<Array<Scalars['String']>>;
  sender_lt?: Maybe<Scalars['String']>;
  sender_lte?: Maybe<Scalars['String']>;
  sender_not?: Maybe<Scalars['String']>;
  sender_not_contains?: Maybe<Scalars['String']>;
  sender_not_ends_with?: Maybe<Scalars['String']>;
  sender_not_in?: Maybe<Array<Scalars['String']>>;
  sender_not_starts_with?: Maybe<Scalars['String']>;
  sender_starts_with?: Maybe<Scalars['String']>;
  streamedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  streamedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  streamedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['String']>;
  token_contains?: Maybe<Scalars['String']>;
  token_ends_with?: Maybe<Scalars['String']>;
  token_gt?: Maybe<Scalars['String']>;
  token_gte?: Maybe<Scalars['String']>;
  token_in?: Maybe<Array<Scalars['String']>>;
  token_lt?: Maybe<Scalars['String']>;
  token_lte?: Maybe<Scalars['String']>;
  token_not?: Maybe<Scalars['String']>;
  token_not_contains?: Maybe<Scalars['String']>;
  token_not_ends_with?: Maybe<Scalars['String']>;
  token_not_in?: Maybe<Array<Scalars['String']>>;
  token_not_starts_with?: Maybe<Scalars['String']>;
  token_starts_with?: Maybe<Scalars['String']>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum Stream_OrderBy {
  CreatedAtBlockNumber = 'createdAtBlockNumber',
  CreatedAtTimestamp = 'createdAtTimestamp',
  CurrentFlowRate = 'currentFlowRate',
  FlowUpdatedEvents = 'flowUpdatedEvents',
  Id = 'id',
  Receiver = 'receiver',
  Sender = 'sender',
  StreamedUntilUpdatedAt = 'streamedUntilUpdatedAt',
  Token = 'token',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type Subscription = {
  __typename?: 'Subscription';
  /** Access to subgraph metadata */
  _meta?: Maybe<_Meta_>;
  account?: Maybe<Account>;
  accountTokenSnapshot?: Maybe<AccountTokenSnapshot>;
  accountTokenSnapshots: Array<AccountTokenSnapshot>;
  accounts: Array<Account>;
  agreementClassRegisteredEvent?: Maybe<AgreementClassRegisteredEvent>;
  agreementClassRegisteredEvents: Array<AgreementClassRegisteredEvent>;
  agreementClassUpdatedEvent?: Maybe<AgreementClassUpdatedEvent>;
  agreementClassUpdatedEvents: Array<AgreementClassUpdatedEvent>;
  agreementLiquidatedByEvent?: Maybe<AgreementLiquidatedByEvent>;
  agreementLiquidatedByEvents: Array<AgreementLiquidatedByEvent>;
  appRegisteredEvent?: Maybe<AppRegisteredEvent>;
  appRegisteredEvents: Array<AppRegisteredEvent>;
  burnedEvent?: Maybe<BurnedEvent>;
  burnedEvents: Array<BurnedEvent>;
  cfav1LiquidationPeriodChangedEvent?: Maybe<CfAv1LiquidationPeriodChangedEvent>;
  cfav1LiquidationPeriodChangedEvents: Array<CfAv1LiquidationPeriodChangedEvent>;
  configChangedEvent?: Maybe<ConfigChangedEvent>;
  configChangedEvents: Array<ConfigChangedEvent>;
  customSuperTokenCreatedEvent?: Maybe<CustomSuperTokenCreatedEvent>;
  customSuperTokenCreatedEvents: Array<CustomSuperTokenCreatedEvent>;
  event?: Maybe<Event>;
  events: Array<Event>;
  flowUpdatedEvent?: Maybe<FlowUpdatedEvent>;
  flowUpdatedEvents: Array<FlowUpdatedEvent>;
  governanceReplacedEvent?: Maybe<GovernanceReplacedEvent>;
  governanceReplacedEvents: Array<GovernanceReplacedEvent>;
  index?: Maybe<Index>;
  indexCreatedEvent?: Maybe<IndexCreatedEvent>;
  indexCreatedEvents: Array<IndexCreatedEvent>;
  indexDistributionClaimedEvent?: Maybe<IndexDistributionClaimedEvent>;
  indexDistributionClaimedEvents: Array<IndexDistributionClaimedEvent>;
  indexSubscribedEvent?: Maybe<IndexSubscribedEvent>;
  indexSubscribedEvents: Array<IndexSubscribedEvent>;
  indexSubscription?: Maybe<IndexSubscription>;
  indexSubscriptions: Array<IndexSubscription>;
  indexUnitsUpdatedEvent?: Maybe<IndexUnitsUpdatedEvent>;
  indexUnitsUpdatedEvents: Array<IndexUnitsUpdatedEvent>;
  indexUnsubscribedEvent?: Maybe<IndexUnsubscribedEvent>;
  indexUnsubscribedEvents: Array<IndexUnsubscribedEvent>;
  indexUpdatedEvent?: Maybe<IndexUpdatedEvent>;
  indexUpdatedEvents: Array<IndexUpdatedEvent>;
  indexes: Array<Index>;
  jailEvent?: Maybe<JailEvent>;
  jailEvents: Array<JailEvent>;
  mintedEvent?: Maybe<MintedEvent>;
  mintedEvents: Array<MintedEvent>;
  rewardAddressChangedEvent?: Maybe<RewardAddressChangedEvent>;
  rewardAddressChangedEvents: Array<RewardAddressChangedEvent>;
  roleAdminChangedEvent?: Maybe<RoleAdminChangedEvent>;
  roleAdminChangedEvents: Array<RoleAdminChangedEvent>;
  roleGrantedEvent?: Maybe<RoleGrantedEvent>;
  roleGrantedEvents: Array<RoleGrantedEvent>;
  roleRevokedEvent?: Maybe<RoleRevokedEvent>;
  roleRevokedEvents: Array<RoleRevokedEvent>;
  sentEvent?: Maybe<SentEvent>;
  sentEvents: Array<SentEvent>;
  stream?: Maybe<Stream>;
  streamRevision?: Maybe<StreamRevision>;
  streamRevisions: Array<StreamRevision>;
  streams: Array<Stream>;
  subscriptionApprovedEvent?: Maybe<SubscriptionApprovedEvent>;
  subscriptionApprovedEvents: Array<SubscriptionApprovedEvent>;
  subscriptionDistributionClaimedEvent?: Maybe<SubscriptionDistributionClaimedEvent>;
  subscriptionDistributionClaimedEvents: Array<SubscriptionDistributionClaimedEvent>;
  subscriptionRevokedEvent?: Maybe<SubscriptionRevokedEvent>;
  subscriptionRevokedEvents: Array<SubscriptionRevokedEvent>;
  subscriptionUnitsUpdatedEvent?: Maybe<SubscriptionUnitsUpdatedEvent>;
  subscriptionUnitsUpdatedEvents: Array<SubscriptionUnitsUpdatedEvent>;
  superTokenCreatedEvent?: Maybe<SuperTokenCreatedEvent>;
  superTokenCreatedEvents: Array<SuperTokenCreatedEvent>;
  superTokenFactoryUpdatedEvent?: Maybe<SuperTokenFactoryUpdatedEvent>;
  superTokenFactoryUpdatedEvents: Array<SuperTokenFactoryUpdatedEvent>;
  superTokenLogicCreatedEvent?: Maybe<SuperTokenLogicCreatedEvent>;
  superTokenLogicCreatedEvents: Array<SuperTokenLogicCreatedEvent>;
  superTokenLogicUpdatedEvent?: Maybe<SuperTokenLogicUpdatedEvent>;
  superTokenLogicUpdatedEvents: Array<SuperTokenLogicUpdatedEvent>;
  token?: Maybe<Token>;
  tokenDowngradedEvent?: Maybe<TokenDowngradedEvent>;
  tokenDowngradedEvents: Array<TokenDowngradedEvent>;
  tokenStatistic?: Maybe<TokenStatistic>;
  tokenStatistics: Array<TokenStatistic>;
  tokenUpgradedEvent?: Maybe<TokenUpgradedEvent>;
  tokenUpgradedEvents: Array<TokenUpgradedEvent>;
  tokens: Array<Token>;
  transferEvent?: Maybe<TransferEvent>;
  transferEvents: Array<TransferEvent>;
  trustedForwarderChangedEvent?: Maybe<TrustedForwarderChangedEvent>;
  trustedForwarderChangedEvents: Array<TrustedForwarderChangedEvent>;
};


export type Subscription_MetaArgs = {
  block?: Maybe<Block_Height>;
};


export type SubscriptionAccountArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAccountTokenSnapshotArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAccountTokenSnapshotsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AccountTokenSnapshot_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AccountTokenSnapshot_Filter>;
};


export type SubscriptionAccountsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Account_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Account_Filter>;
};


export type SubscriptionAgreementClassRegisteredEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAgreementClassRegisteredEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementClassRegisteredEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementClassRegisteredEvent_Filter>;
};


export type SubscriptionAgreementClassUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAgreementClassUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementClassUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementClassUpdatedEvent_Filter>;
};


export type SubscriptionAgreementLiquidatedByEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAgreementLiquidatedByEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AgreementLiquidatedByEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AgreementLiquidatedByEvent_Filter>;
};


export type SubscriptionAppRegisteredEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionAppRegisteredEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<AppRegisteredEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<AppRegisteredEvent_Filter>;
};


export type SubscriptionBurnedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionBurnedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<BurnedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<BurnedEvent_Filter>;
};


export type SubscriptionCfav1LiquidationPeriodChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionCfav1LiquidationPeriodChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<CfAv1LiquidationPeriodChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<CfAv1LiquidationPeriodChangedEvent_Filter>;
};


export type SubscriptionConfigChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionConfigChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<ConfigChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<ConfigChangedEvent_Filter>;
};


export type SubscriptionCustomSuperTokenCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionCustomSuperTokenCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<CustomSuperTokenCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<CustomSuperTokenCreatedEvent_Filter>;
};


export type SubscriptionEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Event_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Event_Filter>;
};


export type SubscriptionFlowUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionFlowUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<FlowUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<FlowUpdatedEvent_Filter>;
};


export type SubscriptionGovernanceReplacedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionGovernanceReplacedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<GovernanceReplacedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<GovernanceReplacedEvent_Filter>;
};


export type SubscriptionIndexArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexCreatedEvent_Filter>;
};


export type SubscriptionIndexDistributionClaimedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexDistributionClaimedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexDistributionClaimedEvent_Filter>;
};


export type SubscriptionIndexSubscribedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexSubscribedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexSubscribedEvent_Filter>;
};


export type SubscriptionIndexSubscriptionArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexSubscriptionsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexSubscription_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexSubscription_Filter>;
};


export type SubscriptionIndexUnitsUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexUnitsUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUnitsUpdatedEvent_Filter>;
};


export type SubscriptionIndexUnsubscribedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexUnsubscribedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUnsubscribedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUnsubscribedEvent_Filter>;
};


export type SubscriptionIndexUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionIndexUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<IndexUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<IndexUpdatedEvent_Filter>;
};


export type SubscriptionIndexesArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Index_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Index_Filter>;
};


export type SubscriptionJailEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionJailEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<JailEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<JailEvent_Filter>;
};


export type SubscriptionMintedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionMintedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<MintedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<MintedEvent_Filter>;
};


export type SubscriptionRewardAddressChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionRewardAddressChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RewardAddressChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RewardAddressChangedEvent_Filter>;
};


export type SubscriptionRoleAdminChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionRoleAdminChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleAdminChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleAdminChangedEvent_Filter>;
};


export type SubscriptionRoleGrantedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionRoleGrantedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleGrantedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleGrantedEvent_Filter>;
};


export type SubscriptionRoleRevokedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionRoleRevokedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<RoleRevokedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<RoleRevokedEvent_Filter>;
};


export type SubscriptionSentEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSentEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SentEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SentEvent_Filter>;
};


export type SubscriptionStreamArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionStreamRevisionArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionStreamRevisionsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<StreamRevision_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<StreamRevision_Filter>;
};


export type SubscriptionStreamsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Stream_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Stream_Filter>;
};


export type SubscriptionSubscriptionApprovedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSubscriptionApprovedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionApprovedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionApprovedEvent_Filter>;
};


export type SubscriptionSubscriptionDistributionClaimedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSubscriptionDistributionClaimedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionDistributionClaimedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionDistributionClaimedEvent_Filter>;
};


export type SubscriptionSubscriptionRevokedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSubscriptionRevokedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionRevokedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionRevokedEvent_Filter>;
};


export type SubscriptionSubscriptionUnitsUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSubscriptionUnitsUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SubscriptionUnitsUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SubscriptionUnitsUpdatedEvent_Filter>;
};


export type SubscriptionSuperTokenCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSuperTokenCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenCreatedEvent_Filter>;
};


export type SubscriptionSuperTokenFactoryUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSuperTokenFactoryUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenFactoryUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenFactoryUpdatedEvent_Filter>;
};


export type SubscriptionSuperTokenLogicCreatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSuperTokenLogicCreatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenLogicCreatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenLogicCreatedEvent_Filter>;
};


export type SubscriptionSuperTokenLogicUpdatedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionSuperTokenLogicUpdatedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<SuperTokenLogicUpdatedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<SuperTokenLogicUpdatedEvent_Filter>;
};


export type SubscriptionTokenArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTokenDowngradedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTokenDowngradedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenDowngradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenDowngradedEvent_Filter>;
};


export type SubscriptionTokenStatisticArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTokenStatisticsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenStatistic_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenStatistic_Filter>;
};


export type SubscriptionTokenUpgradedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTokenUpgradedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TokenUpgradedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TokenUpgradedEvent_Filter>;
};


export type SubscriptionTokensArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<Token_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<Token_Filter>;
};


export type SubscriptionTransferEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTransferEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TransferEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TransferEvent_Filter>;
};


export type SubscriptionTrustedForwarderChangedEventArgs = {
  block?: Maybe<Block_Height>;
  id: Scalars['ID'];
  subgraphError?: _SubgraphErrorPolicy_;
};


export type SubscriptionTrustedForwarderChangedEventsArgs = {
  block?: Maybe<Block_Height>;
  first?: Maybe<Scalars['Int']>;
  orderBy?: Maybe<TrustedForwarderChangedEvent_OrderBy>;
  orderDirection?: Maybe<OrderDirection>;
  skip?: Maybe<Scalars['Int']>;
  subgraphError?: _SubgraphErrorPolicy_;
  where?: Maybe<TrustedForwarderChangedEvent_Filter>;
};

export type SubscriptionApprovedEvent = Event & {
  __typename?: 'SubscriptionApprovedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  subscription: IndexSubscription;
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type SubscriptionApprovedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscription?: Maybe<Scalars['String']>;
  subscription_contains?: Maybe<Scalars['String']>;
  subscription_ends_with?: Maybe<Scalars['String']>;
  subscription_gt?: Maybe<Scalars['String']>;
  subscription_gte?: Maybe<Scalars['String']>;
  subscription_in?: Maybe<Array<Scalars['String']>>;
  subscription_lt?: Maybe<Scalars['String']>;
  subscription_lte?: Maybe<Scalars['String']>;
  subscription_not?: Maybe<Scalars['String']>;
  subscription_not_contains?: Maybe<Scalars['String']>;
  subscription_not_ends_with?: Maybe<Scalars['String']>;
  subscription_not_in?: Maybe<Array<Scalars['String']>>;
  subscription_not_starts_with?: Maybe<Scalars['String']>;
  subscription_starts_with?: Maybe<Scalars['String']>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SubscriptionApprovedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Subscription = 'subscription',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

export type SubscriptionDistributionClaimedEvent = Event & {
  __typename?: 'SubscriptionDistributionClaimedEvent';
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  subscription: IndexSubscription;
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type SubscriptionDistributionClaimedEvent_Filter = {
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscription?: Maybe<Scalars['String']>;
  subscription_contains?: Maybe<Scalars['String']>;
  subscription_ends_with?: Maybe<Scalars['String']>;
  subscription_gt?: Maybe<Scalars['String']>;
  subscription_gte?: Maybe<Scalars['String']>;
  subscription_in?: Maybe<Array<Scalars['String']>>;
  subscription_lt?: Maybe<Scalars['String']>;
  subscription_lte?: Maybe<Scalars['String']>;
  subscription_not?: Maybe<Scalars['String']>;
  subscription_not_contains?: Maybe<Scalars['String']>;
  subscription_not_ends_with?: Maybe<Scalars['String']>;
  subscription_not_in?: Maybe<Array<Scalars['String']>>;
  subscription_not_starts_with?: Maybe<Scalars['String']>;
  subscription_starts_with?: Maybe<Scalars['String']>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SubscriptionDistributionClaimedEvent_OrderBy {
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Id = 'id',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Subscription = 'subscription',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

export type SubscriptionRevokedEvent = Event & {
  __typename?: 'SubscriptionRevokedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  indexId: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  subscription: IndexSubscription;
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  userData: Scalars['Bytes'];
};

export type SubscriptionRevokedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscription?: Maybe<Scalars['String']>;
  subscription_contains?: Maybe<Scalars['String']>;
  subscription_ends_with?: Maybe<Scalars['String']>;
  subscription_gt?: Maybe<Scalars['String']>;
  subscription_gte?: Maybe<Scalars['String']>;
  subscription_in?: Maybe<Array<Scalars['String']>>;
  subscription_lt?: Maybe<Scalars['String']>;
  subscription_lte?: Maybe<Scalars['String']>;
  subscription_not?: Maybe<Scalars['String']>;
  subscription_not_contains?: Maybe<Scalars['String']>;
  subscription_not_ends_with?: Maybe<Scalars['String']>;
  subscription_not_in?: Maybe<Array<Scalars['String']>>;
  subscription_not_starts_with?: Maybe<Scalars['String']>;
  subscription_starts_with?: Maybe<Scalars['String']>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SubscriptionRevokedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  IndexId = 'indexId',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Subscription = 'subscription',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  UserData = 'userData'
}

export type SubscriptionUnitsUpdatedEvent = Event & {
  __typename?: 'SubscriptionUnitsUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  indexId: Scalars['BigInt'];
  oldUnits: Scalars['BigInt'];
  publisher: Scalars['Bytes'];
  subscriber: Scalars['Bytes'];
  subscription: IndexSubscription;
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  units: Scalars['BigInt'];
  userData: Scalars['Bytes'];
};

export type SubscriptionUnitsUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  indexId?: Maybe<Scalars['BigInt']>;
  indexId_gt?: Maybe<Scalars['BigInt']>;
  indexId_gte?: Maybe<Scalars['BigInt']>;
  indexId_in?: Maybe<Array<Scalars['BigInt']>>;
  indexId_lt?: Maybe<Scalars['BigInt']>;
  indexId_lte?: Maybe<Scalars['BigInt']>;
  indexId_not?: Maybe<Scalars['BigInt']>;
  indexId_not_in?: Maybe<Array<Scalars['BigInt']>>;
  oldUnits?: Maybe<Scalars['BigInt']>;
  oldUnits_gt?: Maybe<Scalars['BigInt']>;
  oldUnits_gte?: Maybe<Scalars['BigInt']>;
  oldUnits_in?: Maybe<Array<Scalars['BigInt']>>;
  oldUnits_lt?: Maybe<Scalars['BigInt']>;
  oldUnits_lte?: Maybe<Scalars['BigInt']>;
  oldUnits_not?: Maybe<Scalars['BigInt']>;
  oldUnits_not_in?: Maybe<Array<Scalars['BigInt']>>;
  publisher?: Maybe<Scalars['Bytes']>;
  publisher_contains?: Maybe<Scalars['Bytes']>;
  publisher_in?: Maybe<Array<Scalars['Bytes']>>;
  publisher_not?: Maybe<Scalars['Bytes']>;
  publisher_not_contains?: Maybe<Scalars['Bytes']>;
  publisher_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber?: Maybe<Scalars['Bytes']>;
  subscriber_contains?: Maybe<Scalars['Bytes']>;
  subscriber_in?: Maybe<Array<Scalars['Bytes']>>;
  subscriber_not?: Maybe<Scalars['Bytes']>;
  subscriber_not_contains?: Maybe<Scalars['Bytes']>;
  subscriber_not_in?: Maybe<Array<Scalars['Bytes']>>;
  subscription?: Maybe<Scalars['String']>;
  subscription_contains?: Maybe<Scalars['String']>;
  subscription_ends_with?: Maybe<Scalars['String']>;
  subscription_gt?: Maybe<Scalars['String']>;
  subscription_gte?: Maybe<Scalars['String']>;
  subscription_in?: Maybe<Array<Scalars['String']>>;
  subscription_lt?: Maybe<Scalars['String']>;
  subscription_lte?: Maybe<Scalars['String']>;
  subscription_not?: Maybe<Scalars['String']>;
  subscription_not_contains?: Maybe<Scalars['String']>;
  subscription_not_ends_with?: Maybe<Scalars['String']>;
  subscription_not_in?: Maybe<Array<Scalars['String']>>;
  subscription_not_starts_with?: Maybe<Scalars['String']>;
  subscription_starts_with?: Maybe<Scalars['String']>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  units?: Maybe<Scalars['BigInt']>;
  units_gt?: Maybe<Scalars['BigInt']>;
  units_gte?: Maybe<Scalars['BigInt']>;
  units_in?: Maybe<Array<Scalars['BigInt']>>;
  units_lt?: Maybe<Scalars['BigInt']>;
  units_lte?: Maybe<Scalars['BigInt']>;
  units_not?: Maybe<Scalars['BigInt']>;
  units_not_in?: Maybe<Array<Scalars['BigInt']>>;
  userData?: Maybe<Scalars['Bytes']>;
  userData_contains?: Maybe<Scalars['Bytes']>;
  userData_in?: Maybe<Array<Scalars['Bytes']>>;
  userData_not?: Maybe<Scalars['Bytes']>;
  userData_not_contains?: Maybe<Scalars['Bytes']>;
  userData_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SubscriptionUnitsUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  IndexId = 'indexId',
  OldUnits = 'oldUnits',
  Publisher = 'publisher',
  Subscriber = 'subscriber',
  Subscription = 'subscription',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash',
  Units = 'units',
  UserData = 'userData'
}

export type SuperTokenCreatedEvent = Event & {
  __typename?: 'SuperTokenCreatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type SuperTokenCreatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SuperTokenCreatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

export type SuperTokenFactoryUpdatedEvent = Event & {
  __typename?: 'SuperTokenFactoryUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  newFactory: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type SuperTokenFactoryUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  newFactory?: Maybe<Scalars['Bytes']>;
  newFactory_contains?: Maybe<Scalars['Bytes']>;
  newFactory_in?: Maybe<Array<Scalars['Bytes']>>;
  newFactory_not?: Maybe<Scalars['Bytes']>;
  newFactory_not_contains?: Maybe<Scalars['Bytes']>;
  newFactory_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SuperTokenFactoryUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  NewFactory = 'newFactory',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type SuperTokenLogicCreatedEvent = Event & {
  __typename?: 'SuperTokenLogicCreatedEvent';
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  tokenLogic: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type SuperTokenLogicCreatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  tokenLogic?: Maybe<Scalars['Bytes']>;
  tokenLogic_contains?: Maybe<Scalars['Bytes']>;
  tokenLogic_in?: Maybe<Array<Scalars['Bytes']>>;
  tokenLogic_not?: Maybe<Scalars['Bytes']>;
  tokenLogic_not_contains?: Maybe<Scalars['Bytes']>;
  tokenLogic_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SuperTokenLogicCreatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  TokenLogic = 'tokenLogic',
  TransactionHash = 'transactionHash'
}

export type SuperTokenLogicUpdatedEvent = Event & {
  __typename?: 'SuperTokenLogicUpdatedEvent';
  blockNumber: Scalars['BigInt'];
  code: Scalars['Bytes'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type SuperTokenLogicUpdatedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  code?: Maybe<Scalars['Bytes']>;
  code_contains?: Maybe<Scalars['Bytes']>;
  code_in?: Maybe<Array<Scalars['Bytes']>>;
  code_not?: Maybe<Scalars['Bytes']>;
  code_not_contains?: Maybe<Scalars['Bytes']>;
  code_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum SuperTokenLogicUpdatedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Code = 'code',
  Id = 'id',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

/**
 * Token: A HOL entity created for super tokens that are "valid" (tokens that have
 * Superfluid's host contract address set as the host).
 */
export type Token = {
  __typename?: 'Token';
  createdAtBlockNumber: Scalars['BigInt'];
  createdAtTimestamp: Scalars['BigInt'];
  /** ID: the token address */
  id: Scalars['ID'];
  /** This indicates whether the token is a part of our resolver list. */
  isListed: Scalars['Boolean'];
  isSuperToken: Scalars['Boolean'];
  name: Scalars['String'];
  symbol: Scalars['String'];
  /** The address of the underlying ERC20 token. */
  underlyingAddress: Scalars['Bytes'];
};

export type TokenDowngradedEvent = Event & {
  __typename?: 'TokenDowngradedEvent';
  account: Account;
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type TokenDowngradedEvent_Filter = {
  account?: Maybe<Scalars['String']>;
  account_contains?: Maybe<Scalars['String']>;
  account_ends_with?: Maybe<Scalars['String']>;
  account_gt?: Maybe<Scalars['String']>;
  account_gte?: Maybe<Scalars['String']>;
  account_in?: Maybe<Array<Scalars['String']>>;
  account_lt?: Maybe<Scalars['String']>;
  account_lte?: Maybe<Scalars['String']>;
  account_not?: Maybe<Scalars['String']>;
  account_not_contains?: Maybe<Scalars['String']>;
  account_not_ends_with?: Maybe<Scalars['String']>;
  account_not_in?: Maybe<Array<Scalars['String']>>;
  account_not_starts_with?: Maybe<Scalars['String']>;
  account_starts_with?: Maybe<Scalars['String']>;
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum TokenDowngradedEvent_OrderBy {
  Account = 'account',
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

/** TokenStatistic: An aggregate entity which aggregates data of a single `token`. */
export type TokenStatistic = {
  __typename?: 'TokenStatistic';
  /** ID: tokenID */
  id: Scalars['ID'];
  token: Token;
  /** The all-time total amount distributed until the `updatedAtTimestamp`/`updatedAtBlock`. */
  totalAmountDistributedUntilUpdatedAt: Scalars['BigInt'];
  /** The all-time total amount streamed until the `updatedAtTimestamp`/`updatedAtBlock`. */
  totalAmountStreamedUntilUpdatedAt: Scalars['BigInt'];
  /** The all-time total amount transferred until the `updatedAtTimestamp`/`updatedAtBlock`. */
  totalAmountTransferredUntilUpdatedAt: Scalars['BigInt'];
  /** Counts all approved subscriptions whether or not they have units. */
  totalApprovedSubscriptions: Scalars['Int'];
  /**
   * The total number of "active" (has greater than 0 units and has distributed it at
   * least once) Indexes created with `token`.
   */
  totalNumberOfActiveIndexes: Scalars['Int'];
  /** The total number of currently active `token` streams. */
  totalNumberOfActiveStreams: Scalars['Int'];
  /** The all-time number of closed streams. */
  totalNumberOfClosedStreams: Scalars['Int'];
  /** The total number of Indexes created with `token`. */
  totalNumberOfIndexes: Scalars['Int'];
  /** The total outflow rate of the `token` (how much value is being moved). */
  totalOutflowRate: Scalars['BigInt'];
  /**
   * The number of subscriptions which have units allocated to them
   * created with Indexes that distribute `token`.
   */
  totalSubscriptionsWithUnits: Scalars['Int'];
  /**
   * The total supply of the token - this is impacted by users upgrading/downgrading their
   * tokens.
   */
  totalSupply: Scalars['BigInt'];
  updatedAtBlockNumber: Scalars['BigInt'];
  updatedAtTimestamp: Scalars['BigInt'];
};

export type TokenStatistic_Filter = {
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  token?: Maybe<Scalars['String']>;
  token_contains?: Maybe<Scalars['String']>;
  token_ends_with?: Maybe<Scalars['String']>;
  token_gt?: Maybe<Scalars['String']>;
  token_gte?: Maybe<Scalars['String']>;
  token_in?: Maybe<Array<Scalars['String']>>;
  token_lt?: Maybe<Scalars['String']>;
  token_lte?: Maybe<Scalars['String']>;
  token_not?: Maybe<Scalars['String']>;
  token_not_contains?: Maybe<Scalars['String']>;
  token_not_ends_with?: Maybe<Scalars['String']>;
  token_not_in?: Maybe<Array<Scalars['String']>>;
  token_not_starts_with?: Maybe<Scalars['String']>;
  token_starts_with?: Maybe<Scalars['String']>;
  totalAmountDistributedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountDistributedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountDistributedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountStreamedUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountStreamedUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountStreamedUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountTransferredUntilUpdatedAt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_gt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_gte?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_in?: Maybe<Array<Scalars['BigInt']>>;
  totalAmountTransferredUntilUpdatedAt_lt?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_lte?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_not?: Maybe<Scalars['BigInt']>;
  totalAmountTransferredUntilUpdatedAt_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalApprovedSubscriptions?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_gt?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_gte?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_in?: Maybe<Array<Scalars['Int']>>;
  totalApprovedSubscriptions_lt?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_lte?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_not?: Maybe<Scalars['Int']>;
  totalApprovedSubscriptions_not_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfActiveIndexes?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_gt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_gte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfActiveIndexes_lt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_lte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_not?: Maybe<Scalars['Int']>;
  totalNumberOfActiveIndexes_not_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfActiveStreams?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_gt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_gte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfActiveStreams_lt?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_lte?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_not?: Maybe<Scalars['Int']>;
  totalNumberOfActiveStreams_not_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfClosedStreams?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_gt?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_gte?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfClosedStreams_lt?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_lte?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_not?: Maybe<Scalars['Int']>;
  totalNumberOfClosedStreams_not_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfIndexes?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_gt?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_gte?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_in?: Maybe<Array<Scalars['Int']>>;
  totalNumberOfIndexes_lt?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_lte?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_not?: Maybe<Scalars['Int']>;
  totalNumberOfIndexes_not_in?: Maybe<Array<Scalars['Int']>>;
  totalOutflowRate?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_gt?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_gte?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_in?: Maybe<Array<Scalars['BigInt']>>;
  totalOutflowRate_lt?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_lte?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_not?: Maybe<Scalars['BigInt']>;
  totalOutflowRate_not_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSubscriptionsWithUnits?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_gte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_in?: Maybe<Array<Scalars['Int']>>;
  totalSubscriptionsWithUnits_lt?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_lte?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not?: Maybe<Scalars['Int']>;
  totalSubscriptionsWithUnits_not_in?: Maybe<Array<Scalars['Int']>>;
  totalSupply?: Maybe<Scalars['BigInt']>;
  totalSupply_gt?: Maybe<Scalars['BigInt']>;
  totalSupply_gte?: Maybe<Scalars['BigInt']>;
  totalSupply_in?: Maybe<Array<Scalars['BigInt']>>;
  totalSupply_lt?: Maybe<Scalars['BigInt']>;
  totalSupply_lte?: Maybe<Scalars['BigInt']>;
  totalSupply_not?: Maybe<Scalars['BigInt']>;
  totalSupply_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  updatedAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  updatedAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  updatedAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum TokenStatistic_OrderBy {
  Id = 'id',
  Token = 'token',
  TotalAmountDistributedUntilUpdatedAt = 'totalAmountDistributedUntilUpdatedAt',
  TotalAmountStreamedUntilUpdatedAt = 'totalAmountStreamedUntilUpdatedAt',
  TotalAmountTransferredUntilUpdatedAt = 'totalAmountTransferredUntilUpdatedAt',
  TotalApprovedSubscriptions = 'totalApprovedSubscriptions',
  TotalNumberOfActiveIndexes = 'totalNumberOfActiveIndexes',
  TotalNumberOfActiveStreams = 'totalNumberOfActiveStreams',
  TotalNumberOfClosedStreams = 'totalNumberOfClosedStreams',
  TotalNumberOfIndexes = 'totalNumberOfIndexes',
  TotalOutflowRate = 'totalOutflowRate',
  TotalSubscriptionsWithUnits = 'totalSubscriptionsWithUnits',
  TotalSupply = 'totalSupply',
  UpdatedAtBlockNumber = 'updatedAtBlockNumber',
  UpdatedAtTimestamp = 'updatedAtTimestamp'
}

export type TokenUpgradedEvent = Event & {
  __typename?: 'TokenUpgradedEvent';
  account: Account;
  amount: Scalars['BigInt'];
  blockNumber: Scalars['BigInt'];
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
};

export type TokenUpgradedEvent_Filter = {
  account?: Maybe<Scalars['String']>;
  account_contains?: Maybe<Scalars['String']>;
  account_ends_with?: Maybe<Scalars['String']>;
  account_gt?: Maybe<Scalars['String']>;
  account_gte?: Maybe<Scalars['String']>;
  account_in?: Maybe<Array<Scalars['String']>>;
  account_lt?: Maybe<Scalars['String']>;
  account_lte?: Maybe<Scalars['String']>;
  account_not?: Maybe<Scalars['String']>;
  account_not_contains?: Maybe<Scalars['String']>;
  account_not_ends_with?: Maybe<Scalars['String']>;
  account_not_in?: Maybe<Array<Scalars['String']>>;
  account_not_starts_with?: Maybe<Scalars['String']>;
  account_starts_with?: Maybe<Scalars['String']>;
  amount?: Maybe<Scalars['BigInt']>;
  amount_gt?: Maybe<Scalars['BigInt']>;
  amount_gte?: Maybe<Scalars['BigInt']>;
  amount_in?: Maybe<Array<Scalars['BigInt']>>;
  amount_lt?: Maybe<Scalars['BigInt']>;
  amount_lte?: Maybe<Scalars['BigInt']>;
  amount_not?: Maybe<Scalars['BigInt']>;
  amount_not_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum TokenUpgradedEvent_OrderBy {
  Account = 'account',
  Amount = 'amount',
  BlockNumber = 'blockNumber',
  Id = 'id',
  Timestamp = 'timestamp',
  Token = 'token',
  TransactionHash = 'transactionHash'
}

export type Token_Filter = {
  createdAtBlockNumber?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_gte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtBlockNumber_lt?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_lte?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not?: Maybe<Scalars['BigInt']>;
  createdAtBlockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_gte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  createdAtTimestamp_lt?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_lte?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not?: Maybe<Scalars['BigInt']>;
  createdAtTimestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isListed?: Maybe<Scalars['Boolean']>;
  isListed_in?: Maybe<Array<Scalars['Boolean']>>;
  isListed_not?: Maybe<Scalars['Boolean']>;
  isListed_not_in?: Maybe<Array<Scalars['Boolean']>>;
  isSuperToken?: Maybe<Scalars['Boolean']>;
  isSuperToken_in?: Maybe<Array<Scalars['Boolean']>>;
  isSuperToken_not?: Maybe<Scalars['Boolean']>;
  isSuperToken_not_in?: Maybe<Array<Scalars['Boolean']>>;
  name?: Maybe<Scalars['String']>;
  name_contains?: Maybe<Scalars['String']>;
  name_ends_with?: Maybe<Scalars['String']>;
  name_gt?: Maybe<Scalars['String']>;
  name_gte?: Maybe<Scalars['String']>;
  name_in?: Maybe<Array<Scalars['String']>>;
  name_lt?: Maybe<Scalars['String']>;
  name_lte?: Maybe<Scalars['String']>;
  name_not?: Maybe<Scalars['String']>;
  name_not_contains?: Maybe<Scalars['String']>;
  name_not_ends_with?: Maybe<Scalars['String']>;
  name_not_in?: Maybe<Array<Scalars['String']>>;
  name_not_starts_with?: Maybe<Scalars['String']>;
  name_starts_with?: Maybe<Scalars['String']>;
  symbol?: Maybe<Scalars['String']>;
  symbol_contains?: Maybe<Scalars['String']>;
  symbol_ends_with?: Maybe<Scalars['String']>;
  symbol_gt?: Maybe<Scalars['String']>;
  symbol_gte?: Maybe<Scalars['String']>;
  symbol_in?: Maybe<Array<Scalars['String']>>;
  symbol_lt?: Maybe<Scalars['String']>;
  symbol_lte?: Maybe<Scalars['String']>;
  symbol_not?: Maybe<Scalars['String']>;
  symbol_not_contains?: Maybe<Scalars['String']>;
  symbol_not_ends_with?: Maybe<Scalars['String']>;
  symbol_not_in?: Maybe<Array<Scalars['String']>>;
  symbol_not_starts_with?: Maybe<Scalars['String']>;
  symbol_starts_with?: Maybe<Scalars['String']>;
  underlyingAddress?: Maybe<Scalars['Bytes']>;
  underlyingAddress_contains?: Maybe<Scalars['Bytes']>;
  underlyingAddress_in?: Maybe<Array<Scalars['Bytes']>>;
  underlyingAddress_not?: Maybe<Scalars['Bytes']>;
  underlyingAddress_not_contains?: Maybe<Scalars['Bytes']>;
  underlyingAddress_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum Token_OrderBy {
  CreatedAtBlockNumber = 'createdAtBlockNumber',
  CreatedAtTimestamp = 'createdAtTimestamp',
  Id = 'id',
  IsListed = 'isListed',
  IsSuperToken = 'isSuperToken',
  Name = 'name',
  Symbol = 'symbol',
  UnderlyingAddress = 'underlyingAddress'
}

export type TransferEvent = Event & {
  __typename?: 'TransferEvent';
  blockNumber: Scalars['BigInt'];
  from: Account;
  id: Scalars['ID'];
  timestamp: Scalars['BigInt'];
  to: Account;
  token: Scalars['Bytes'];
  transactionHash: Scalars['Bytes'];
  value: Scalars['BigInt'];
};

export type TransferEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  from?: Maybe<Scalars['String']>;
  from_contains?: Maybe<Scalars['String']>;
  from_ends_with?: Maybe<Scalars['String']>;
  from_gt?: Maybe<Scalars['String']>;
  from_gte?: Maybe<Scalars['String']>;
  from_in?: Maybe<Array<Scalars['String']>>;
  from_lt?: Maybe<Scalars['String']>;
  from_lte?: Maybe<Scalars['String']>;
  from_not?: Maybe<Scalars['String']>;
  from_not_contains?: Maybe<Scalars['String']>;
  from_not_ends_with?: Maybe<Scalars['String']>;
  from_not_in?: Maybe<Array<Scalars['String']>>;
  from_not_starts_with?: Maybe<Scalars['String']>;
  from_starts_with?: Maybe<Scalars['String']>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  to?: Maybe<Scalars['String']>;
  to_contains?: Maybe<Scalars['String']>;
  to_ends_with?: Maybe<Scalars['String']>;
  to_gt?: Maybe<Scalars['String']>;
  to_gte?: Maybe<Scalars['String']>;
  to_in?: Maybe<Array<Scalars['String']>>;
  to_lt?: Maybe<Scalars['String']>;
  to_lte?: Maybe<Scalars['String']>;
  to_not?: Maybe<Scalars['String']>;
  to_not_contains?: Maybe<Scalars['String']>;
  to_not_ends_with?: Maybe<Scalars['String']>;
  to_not_in?: Maybe<Array<Scalars['String']>>;
  to_not_starts_with?: Maybe<Scalars['String']>;
  to_starts_with?: Maybe<Scalars['String']>;
  token?: Maybe<Scalars['Bytes']>;
  token_contains?: Maybe<Scalars['Bytes']>;
  token_in?: Maybe<Array<Scalars['Bytes']>>;
  token_not?: Maybe<Scalars['Bytes']>;
  token_not_contains?: Maybe<Scalars['Bytes']>;
  token_not_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
  value?: Maybe<Scalars['BigInt']>;
  value_gt?: Maybe<Scalars['BigInt']>;
  value_gte?: Maybe<Scalars['BigInt']>;
  value_in?: Maybe<Array<Scalars['BigInt']>>;
  value_lt?: Maybe<Scalars['BigInt']>;
  value_lte?: Maybe<Scalars['BigInt']>;
  value_not?: Maybe<Scalars['BigInt']>;
  value_not_in?: Maybe<Array<Scalars['BigInt']>>;
};

export enum TransferEvent_OrderBy {
  BlockNumber = 'blockNumber',
  From = 'from',
  Id = 'id',
  Timestamp = 'timestamp',
  To = 'to',
  Token = 'token',
  TransactionHash = 'transactionHash',
  Value = 'value'
}

export type TrustedForwarderChangedEvent = Event & {
  __typename?: 'TrustedForwarderChangedEvent';
  blockNumber: Scalars['BigInt'];
  enabled: Scalars['Boolean'];
  forwarder: Scalars['Bytes'];
  host: Scalars['Bytes'];
  id: Scalars['ID'];
  isSet: Scalars['Boolean'];
  superToken: Scalars['Bytes'];
  timestamp: Scalars['BigInt'];
  transactionHash: Scalars['Bytes'];
};

export type TrustedForwarderChangedEvent_Filter = {
  blockNumber?: Maybe<Scalars['BigInt']>;
  blockNumber_gt?: Maybe<Scalars['BigInt']>;
  blockNumber_gte?: Maybe<Scalars['BigInt']>;
  blockNumber_in?: Maybe<Array<Scalars['BigInt']>>;
  blockNumber_lt?: Maybe<Scalars['BigInt']>;
  blockNumber_lte?: Maybe<Scalars['BigInt']>;
  blockNumber_not?: Maybe<Scalars['BigInt']>;
  blockNumber_not_in?: Maybe<Array<Scalars['BigInt']>>;
  enabled?: Maybe<Scalars['Boolean']>;
  enabled_in?: Maybe<Array<Scalars['Boolean']>>;
  enabled_not?: Maybe<Scalars['Boolean']>;
  enabled_not_in?: Maybe<Array<Scalars['Boolean']>>;
  forwarder?: Maybe<Scalars['Bytes']>;
  forwarder_contains?: Maybe<Scalars['Bytes']>;
  forwarder_in?: Maybe<Array<Scalars['Bytes']>>;
  forwarder_not?: Maybe<Scalars['Bytes']>;
  forwarder_not_contains?: Maybe<Scalars['Bytes']>;
  forwarder_not_in?: Maybe<Array<Scalars['Bytes']>>;
  host?: Maybe<Scalars['Bytes']>;
  host_contains?: Maybe<Scalars['Bytes']>;
  host_in?: Maybe<Array<Scalars['Bytes']>>;
  host_not?: Maybe<Scalars['Bytes']>;
  host_not_contains?: Maybe<Scalars['Bytes']>;
  host_not_in?: Maybe<Array<Scalars['Bytes']>>;
  id?: Maybe<Scalars['ID']>;
  id_gt?: Maybe<Scalars['ID']>;
  id_gte?: Maybe<Scalars['ID']>;
  id_in?: Maybe<Array<Scalars['ID']>>;
  id_lt?: Maybe<Scalars['ID']>;
  id_lte?: Maybe<Scalars['ID']>;
  id_not?: Maybe<Scalars['ID']>;
  id_not_in?: Maybe<Array<Scalars['ID']>>;
  isSet?: Maybe<Scalars['Boolean']>;
  isSet_in?: Maybe<Array<Scalars['Boolean']>>;
  isSet_not?: Maybe<Scalars['Boolean']>;
  isSet_not_in?: Maybe<Array<Scalars['Boolean']>>;
  superToken?: Maybe<Scalars['Bytes']>;
  superToken_contains?: Maybe<Scalars['Bytes']>;
  superToken_in?: Maybe<Array<Scalars['Bytes']>>;
  superToken_not?: Maybe<Scalars['Bytes']>;
  superToken_not_contains?: Maybe<Scalars['Bytes']>;
  superToken_not_in?: Maybe<Array<Scalars['Bytes']>>;
  timestamp?: Maybe<Scalars['BigInt']>;
  timestamp_gt?: Maybe<Scalars['BigInt']>;
  timestamp_gte?: Maybe<Scalars['BigInt']>;
  timestamp_in?: Maybe<Array<Scalars['BigInt']>>;
  timestamp_lt?: Maybe<Scalars['BigInt']>;
  timestamp_lte?: Maybe<Scalars['BigInt']>;
  timestamp_not?: Maybe<Scalars['BigInt']>;
  timestamp_not_in?: Maybe<Array<Scalars['BigInt']>>;
  transactionHash?: Maybe<Scalars['Bytes']>;
  transactionHash_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_in?: Maybe<Array<Scalars['Bytes']>>;
  transactionHash_not?: Maybe<Scalars['Bytes']>;
  transactionHash_not_contains?: Maybe<Scalars['Bytes']>;
  transactionHash_not_in?: Maybe<Array<Scalars['Bytes']>>;
};

export enum TrustedForwarderChangedEvent_OrderBy {
  BlockNumber = 'blockNumber',
  Enabled = 'enabled',
  Forwarder = 'forwarder',
  Host = 'host',
  Id = 'id',
  IsSet = 'isSet',
  SuperToken = 'superToken',
  Timestamp = 'timestamp',
  TransactionHash = 'transactionHash'
}

export type _Block_ = {
  __typename?: '_Block_';
  /** The hash of the block */
  hash?: Maybe<Scalars['Bytes']>;
  /** The block number */
  number: Scalars['Int'];
};

/** The type for the top-level _meta field */
export type _Meta_ = {
  __typename?: '_Meta_';
  /**
   * Information about a specific subgraph block. The hash of the block
   * will be null if the _meta field has a block constraint that asks for
   * a block number. It will be filled if the _meta field has no block constraint
   * and therefore asks for the latest  block
   */
  block: _Block_;
  /** The deployment ID */
  deployment: Scalars['String'];
  /** If `true`, the subgraph encountered indexing errors at some past block */
  hasIndexingErrors: Scalars['Boolean'];
};

export enum _SubgraphErrorPolicy_ {
  /** Data will be returned even if the subgraph has indexing errors */
  Allow = 'allow',
  /** If the subgraph has indexing errors, data will be omitted. The default. */
  Deny = 'deny'
}
