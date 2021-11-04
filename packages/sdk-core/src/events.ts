export type EventBase = {
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
};

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
    | TokenUpgradedEvent;

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
    // TODO(KK): Actually account events
    | SubscriptionApprovedEvent
    | SubscriptionDistributionClaimedEvent
    | SubscriptionRevokedEvent
    | SubscriptionUnitsUpdatedEvent
    | SuperTokenCreatedEvent
    | SuperTokenFactoryUpdatedEvent
    | SuperTokenLogicCreatedEvent
    | SuperTokenLogicUpdatedEvent
    | TrustedForwarderChangedEvent;

export type AllEvents = AccountEvents | OtherEvents;

export interface FlowUpdatedEvent extends EventBase {
    __typename: "FlowUpdatedEvent";
    token: string;
    sender: string;
    receiver: string;
    flowRate: string;
}

export interface IndexCreatedEvent extends EventBase {
    __typename: "IndexCreatedEvent";
    token: string;
    publisher: string;
    indexId: string;
    userData: string;
}

export interface IndexDistributionClaimedEvent extends EventBase {
    __typename: "IndexDistributionClaimedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    amount: string;
}

export interface IndexSubscribedEvent extends EventBase {
    __typename: "IndexSubscribedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
}

export interface IndexUnitsUpdatedEvent extends EventBase {
    __typename: "IndexUnitsUpdatedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    units: string;
    userData: string;
    oldUnits: string;
}

export interface IndexUnsubscribedEvent extends EventBase {
    __typename: "IndexUnsubscribedEvent";
    token: string;
    publisher: string;
    indexId: string;
    subscriber: string;
    userData: string;
}

export interface IndexUpdatedEvent extends EventBase {
    __typename: "IndexUpdatedEvent";
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
    __typename: "TokenDowngradedEvent";
    token: string;
    amount: string;
}

export interface TokenUpgradedEvent extends EventBase {
    __typename: "TokenUpgradedEvent";
    token: string;
    amount: string;
}

export interface TransferEvent extends EventBase {
    __typename: "TransferEvent";
    from: string;
    to: string;
    value: string;
    token: string;
}

export interface AgreementClassRegisteredEvent extends EventBase {
    __typename: "AgreementClassRegisteredEvent";
}

export interface AgreementClassUpdatedEvent extends EventBase {
    __typename: "AgreementClassUpdatedEvent";
}

export interface AgreementLiquidatedByEvent extends EventBase {
    __typename: "AgreementLiquidatedByEvent";
}

export interface AppRegisteredEvent extends EventBase {
    __typename: "AppRegisteredEvent";
}

export interface BurnedEvent extends EventBase {
    __typename: "BurnedEvent";
}

export interface CFAv1LiquidationPeriodChangedEvent extends EventBase {
    __typename: "CFAv1LiquidationPeriodChangedEvent";
}

export interface ConfigChangedEvent extends EventBase {
    __typename: "ConfigChangedEvent";
}

export interface CustomSuperTokenCreatedEvent extends EventBase {
    __typename: "CustomSuperTokenCreatedEvent";
}

export interface GovernanceReplacedEvent extends EventBase {
    __typename: "GovernanceReplacedEvent";
}

export interface JailEvent extends EventBase {
    __typename: "JailEvent";
}

export interface MintedEvent extends EventBase {
    __typename: "MintedEvent";
}

export interface RewardAddressChangedEvent extends EventBase {
    __typename: "RewardAddressChangedEvent";
}

export interface RoleAdminChangedEvent extends EventBase {
    __typename: "RoleAdminChangedEvent";
}

export interface RoleGrantedEvent extends EventBase {
    __typename: "RoleGrantedEvent";
}

export interface RoleRevokedEvent extends EventBase {
    __typename: "RoleRevokedEvent";
}

export interface SentEvent extends EventBase {
    __typename: "SentEvent";
}

export interface SubscriptionApprovedEvent extends EventBase {
    __typename: "SubscriptionApprovedEvent";
}

export interface SubscriptionDistributionClaimedEvent extends EventBase {
    __typename: "SubscriptionDistributionClaimedEvent";
}

export interface SubscriptionRevokedEvent extends EventBase {
    __typename: "SubscriptionRevokedEvent";
}

export interface SubscriptionUnitsUpdatedEvent extends EventBase {
    __typename: "SubscriptionUnitsUpdatedEvent";
}

export interface SuperTokenCreatedEvent extends EventBase {
    __typename: "SuperTokenCreatedEvent";
}

export interface SuperTokenFactoryUpdatedEvent extends EventBase {
    __typename: "SuperTokenFactoryUpdatedEvent";
}

export interface SuperTokenLogicCreatedEvent extends EventBase {
    __typename: "SuperTokenLogicCreatedEvent";
}

export interface SuperTokenLogicUpdatedEvent extends EventBase {
    __typename: "SuperTokenLogicUpdatedEvent";
}

export interface TrustedForwarderChangedEvent extends EventBase {
    __typename: "TrustedForwarderChangedEvent";
}
