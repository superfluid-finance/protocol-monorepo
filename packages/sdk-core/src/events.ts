export type EventBase = {
    id: string;
    blockNumber: number;
    transactionHash: string;
    order: number;
    timestamp: number;
    logIndex: number;
};

export interface IEventFilter {
    readonly account?: string;
    readonly timestamp_gt?: number;
}

export type AccountEvents =
    | FlowUpdatedEvent
    | FlowOperatorUpdatedEvent
    | IndexCreatedEvent
    | IndexDistributionClaimedEvent
    | IndexSubscribedEvent
    | IndexUnitsUpdatedEvent
    | IndexUnsubscribedEvent
    | IndexUpdatedEvent
    | TokenDowngradedEvent
    | TransferEvent
    | TokenUpgradedEvent
    | SubscriptionApprovedEvent
    | SubscriptionDistributionClaimedEvent
    | SubscriptionRevokedEvent
    | SubscriptionUnitsUpdatedEvent;

export type OtherEvents =
    | AgreementClassRegisteredEvent
    | AgreementClassUpdatedEvent
    | AgreementLiquidatedByEvent
    | AgreementLiquidatedV2Event
    | AppRegisteredEvent
    | BurnedEvent
    | CFAv1LiquidationPeriodChangedEvent
    | ConfigChangedEvent
    | CustomSuperTokenCreatedEvent
    | GovernanceReplacedEvent
    | JailEvent
    | MintedEvent
    | RewardAddressChangedEvent
    | RoleAdminChangedEvent
    | RoleGrantedEvent
    | RoleRevokedEvent
    | SentEvent
    | SuperTokenCreatedEvent
    | SuperTokenFactoryUpdatedEvent
    | SuperTokenLogicCreatedEvent
    | SuperTokenLogicUpdatedEvent
    | PPPConfigurationChangedEvent
    | TrustedForwarderChangedEvent
    | UnknownEvent;

export type AllEvents = AccountEvents | OtherEvents;

export interface FlowUpdatedEvent extends EventBase {
    name: "FlowUpdated";
    token: string;
    sender: string;
    receiver: string;
    flowRate: string;
    flowOperator: string;
    deposit: string;
}

export interface FlowOperatorUpdatedEvent extends EventBase {
    name: "FlowOperatorUpdated";
    token: string;
    sender: string;
    permissions: number;
    flowRateAllowance: string;
}

export interface IndexCreatedEvent extends EventBase {
    name: "IndexCreated";
    token: string;
    publisher: string;
    indexId: string;
    userData: string;
}

export interface IndexDistributionClaimedEvent extends EventBase {
    name: "IndexDistributionClaimed";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    amount: string;
}

export interface IndexSubscribedEvent extends EventBase {
    name: "IndexSubscribed";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
}

export interface IndexUnitsUpdatedEvent extends EventBase {
    name: "IndexUnitsUpdated";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    units: string;
    userData: string;
    oldUnits: string;
}

export interface IndexUnsubscribedEvent extends EventBase {
    name: "IndexUnsubscribed";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    userData: string;
}

export interface IndexUpdatedEvent extends EventBase {
    name: "IndexUpdated";
    token: string;
    publisher: string;
    indexId: string;
    oldIndexValue: string;
    newIndexValue: string;
    totalUnitsPending: string;
    totalUnitsApproved: string;
    userData: string;
}

export interface TokenDowngradedEvent extends EventBase {
    name: "TokenDowngraded";
    token: string;
    amount: string;
    account: string;
}

export interface TokenUpgradedEvent extends EventBase {
    name: "TokenUpgraded";
    token: string;
    amount: string;
    account: string;
}

export interface TransferEvent extends EventBase {
    name: "Transfer";
    from: string;
    to: string;
    value: string;
    token: string;
}

export interface AgreementClassRegisteredEvent extends EventBase {
    name: "AgreementClassRegistered";
    agreementType: string;
    code: string;
}

export interface AgreementClassUpdatedEvent extends EventBase {
    name: "AgreementClassUpdated";
    agreementType: string;
    code: string;
}

export interface AgreementLiquidatedByEvent extends EventBase {
    name: "AgreementLiquidatedBy";
    agreementClass: string;
    agreementId: string;
    bailoutAmount: string;
    bondAccount: string;
    liquidatorAccount: string;
    penaltyAccount: string;
    rewardAmount: string;
    token: string;
}

export interface AgreementLiquidatedV2Event extends EventBase {
    name: "AgreementLiquidatedV2";
    token: string;
    liquidatorAccount: string;
    agreementClass: string;
    agreementId: string;
    targetAccount: string;
    rewardAmountReceiver: string;
    rewardAmount: string;
    targetAccountBalanceDelta: string;
    version: string;
    liquidationType: number;

    // TO BE DEPRECATED in v2 endpoint - use rewardAmountReceiver instead
    rewardAccount: string;
}

export interface AppRegisteredEvent extends EventBase {
    name: "AppRegistered";
    app: string;
}

export interface BurnedEvent extends EventBase {
    name: "Burned";
    amount: string;
    data: string;
    from: string;
    operator: string;
    operatorData: string;
}

export interface CFAv1LiquidationPeriodChangedEvent extends EventBase {
    name: "CFAv1LiquidationPeriodChanged";
    host: string;
    isKeySet: boolean;
    liquidationPeriod: number;
    superToken: string;
}

export interface ConfigChangedEvent extends EventBase {
    name: "ConfigChanged";
    host: string;
    isKeySet: boolean;
    key: string;
    superToken: string;
    value: string;
}

export interface CustomSuperTokenCreatedEvent extends EventBase {
    name: "CustomSuperTokenCreated";
    token: string;
}

export interface GovernanceReplacedEvent extends EventBase {
    name: "GovernanceReplaced";
    oldGovernance: string;
    newGovernance: string;
}

export interface JailEvent extends EventBase {
    name: "Jail";
    app: string;
    reason: string;
}

export interface MintedEvent extends EventBase {
    name: "Minted";
    operator: string;
    to: string;
    amount: string;
    data: string;
    operatorData: string;
}

export interface RewardAddressChangedEvent extends EventBase {
    name: "RewardAddressChanged";
    host: string;
    superToken: string;
    isKeySet: boolean;
    rewardAddress: string;
}

export interface RoleAdminChangedEvent extends EventBase {
    name: "RoleAdminChanged";
    role: string;
    previousAdminRole: string;
    newAdminRole: string;
}

export interface RoleGrantedEvent extends EventBase {
    name: "RoleGranted";
    role: string;
    account: string;
    sender: string;
}

export interface RoleRevokedEvent extends EventBase {
    name: "RoleRevoked";
    role: string;
    account: string;
    sender: string;
}

export interface SentEvent extends EventBase {
    name: "Sent";
    operator: string;
    to: string;
    amount: string;
    data: string;
    operatorData: string;
}

export interface SubscriptionApprovedEvent extends EventBase {
    name: "SubscriptionApproved";
    token: string;
    subscriber: string;
    publisher: string;
    indexId: string;
    userData: string;
    subscription: string;
}

export interface SubscriptionDistributionClaimedEvent extends EventBase {
    name: "SubscriptionDistributionClaimed";
    token: string;
    subscriber: string;
    publisher: string;
    indexId: string;
    amount: string;
    subscription: string;
}

export interface SubscriptionRevokedEvent extends EventBase {
    name: "SubscriptionRevoked";
    token: string;
    subscriber: string;
    publisher: string;
    indexId: string;
    userData: string;
    subscription: string;
}

export interface SubscriptionUnitsUpdatedEvent extends EventBase {
    name: "SubscriptionUnitsUpdated";
    token: string;
    subscriber: string;
    publisher: string;
    indexId: string;
    units: string;
    userData: string;
    subscription: string;
}

export interface SuperTokenCreatedEvent extends EventBase {
    name: "SuperTokenCreated";
    token: string;
}

export interface SuperTokenFactoryUpdatedEvent extends EventBase {
    name: "SuperTokenFactoryUpdated";
    newFactory: string;
}

export interface SuperTokenLogicCreatedEvent extends EventBase {
    name: "SuperTokenLogicCreated";
    tokenLogic: string;
}

export interface SuperTokenLogicUpdatedEvent extends EventBase {
    name: "SuperTokenLogicUpdated";
    token: string;
    code: string;
}

export interface PPPConfigurationChangedEvent extends EventBase {
    name: "PPPConfigurationChanged";
    host: string;
    superToken: string;
    isKeySet: boolean;
    liquidationPeriod: string;
    patricianPeriod: string;
}

export interface TrustedForwarderChangedEvent extends EventBase {
    name: "TrustedForwarderChanged";
    host: string;
    superToken: string;
    isKeySet: boolean;
    forwarder: string;
    enabled: boolean;
}

export interface UnknownEvent extends EventBase {
    name: "_Unknown";
}
