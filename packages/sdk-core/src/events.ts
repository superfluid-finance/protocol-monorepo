export interface FlowUpdatedEvent {
    __typename: "FlowUpdatedEvent"
    token: string
    sender: string
    receiver: string
    flowRate: string
}

export interface IndexCreatedEvent {
    __typename: "IndexCreatedEvent"
    token: string
    publisher: string
    indexId: string
    userData: string
}

export interface IndexDistributionClaimedEvent {
    __typename: "IndexDistributionClaimedEvent"
    token: string
    publisher: string
    indexId: string
    subscriber: string
    amount: string
}

export interface IndexSubscribedEvent {
    __typename: "IndexSubscribedEvent"
    token: string
    publisher: string
    indexId: string
    subscriber: string
}

export interface IndexUnitsUpdatedEvent {
    __typename: "IndexUnitsUpdatedEvent"
    token: string
    publisher: string
    indexId: string
    subscriber: string
    units: string
    userData: string
    oldUnits: string
}

export interface IndexUnsubscribedEvent {
    __typename: "IndexUnsubscribedEvent"
    token: string
    publisher: string
    indexId: string
    subscriber: string
    userData: string
}

export interface IndexUpdatedEvent {
    __typename: "IndexUpdatedEvent"
    token: string
    publisher: string
    indexId: string
    oldIndexValue: string
    newIndexValue: string
    totalUnitsPending: string
    totalUnitsApproved: string
    userData: string
}

export interface TokenDowngradedEvent {
    __typename: "TokenDowngradedEvent"
    token: string
    amount: string
}

export interface TokenUpgradedEvent {
    __typename: "TokenUpgradedEvent"
    token: string
    amount: string
}

export interface TransferEvent {
    __typename: "TransferEvent"
    from: string
    to: string
    value: string
    token: string
}

export type Events = FlowUpdatedEvent | IndexCreatedEvent | IndexUpdatedEvent | IndexDistributionClaimedEvent | IndexSubscribedEvent | IndexUnitsUpdatedEvent | IndexUnsubscribedEvent | TokenDowngradedEvent | TokenUpgradedEvent | TransferEvent
