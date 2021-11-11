export type EventBase = {
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

export interface IEventFilter {
    readonly account?: string;
    readonly timestamp_gte?: string;
}

export type AccountEvents =
    | FlowUpdatedEvent
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
    | TrustedForwarderChangedEvent;

export type AllEvents = AccountEvents | OtherEvents;

export interface FlowUpdatedEvent extends EventBase {
    name: "FlowUpdated";
    token: string;
    sender: string;
    receiver: string;
    flowRate: string;
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
}

export interface TokenUpgradedEvent extends EventBase {
    name: "TokenUpgraded";
    token: string;
    amount: string;
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
}

export interface AgreementClassUpdatedEvent extends EventBase {
    name: "AgreementClassUpdated";
}

export interface AgreementLiquidatedByEvent extends EventBase {
    name: "AgreementLiquidatedBy";
}

export interface AppRegisteredEvent extends EventBase {
    name: "AppRegistered";
}

export interface BurnedEvent extends EventBase {
    name: "Burned";
}

export interface CFAv1LiquidationPeriodChangedEvent extends EventBase {
    name: "CFAv1LiquidationPeriodChanged";
}

export interface ConfigChangedEvent extends EventBase {
    name: "ConfigChanged";
}

export interface CustomSuperTokenCreatedEvent extends EventBase {
    name: "CustomSuperTokenCreated";
}

export interface GovernanceReplacedEvent extends EventBase {
    name: "GovernanceReplaced";
}

export interface JailEvent extends EventBase {
    name: "Jail";
}

export interface MintedEvent extends EventBase {
    name: "Minted";
}

export interface RewardAddressChangedEvent extends EventBase {
    name: "RewardAddressChanged";
}

export interface RoleAdminChangedEvent extends EventBase {
    name: "RoleAdminChanged";
}

export interface RoleGrantedEvent extends EventBase {
    name: "RoleGranted";
}

export interface RoleRevokedEvent extends EventBase {
    name: "RoleRevoked";
}

export interface SentEvent extends EventBase {
    name: "Sent";
}

export interface SubscriptionApprovedEvent extends EventBase {
    name: "SubscriptionApproved";
}

export interface SubscriptionDistributionClaimedEvent extends EventBase {
    name: "SubscriptionDistributionClaimed";
}

export interface SubscriptionRevokedEvent extends EventBase {
    name: "SubscriptionRevoked";
}

export interface SubscriptionUnitsUpdatedEvent extends EventBase {
    name: "SubscriptionUnitsUpdated";
}

export interface SuperTokenCreatedEvent extends EventBase {
    name: "SuperTokenCreated";
}

export interface SuperTokenFactoryUpdatedEvent extends EventBase {
    name: "SuperTokenFactoryUpdated";
}

export interface SuperTokenLogicCreatedEvent extends EventBase {
    name: "SuperTokenLogicCreated";
}

export interface SuperTokenLogicUpdatedEvent extends EventBase {
    name: "SuperTokenLogicUpdated";
}

export interface TrustedForwarderChangedEvent extends EventBase {
    name: "TrustedForwarderChanged";
}
