import { gql } from "graphql-request";

// CFA Event Queries
export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents($transactionHash: Bytes!) {
        response: flowUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            timestamp
            transactionHash
            blockNumber
            token
            sender
            receiver
            flowRate
            totalAmountStreamedUntilTimestamp
            totalSenderFlowRate
            totalReceiverFlowRate
            oldFlowRate
            type
        }
    }
`;

// IDA Event Queries

export const getIndexCreatedEvents = gql`
    query getIndexCreatedEvents($transactionHash: Bytes!) {
        response: indexCreatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            userData
            index {
                id
            }
        }
    }
`;
export const getIndexUpdatedEvents = gql`
    query getIndexUpdatedEvents($transactionHash: Bytes!) {
        response: indexUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            oldIndexValue
            newIndexValue
            totalUnitsPending
            totalUnitsApproved
            userData
            index {
                id
            }
        }
    }
`;

export const getIndexSubscribedEvents = gql`
    query getIndexSubscribedEvents($transactionHash: Bytes!) {
        response: indexSubscribedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            subscriber
            userData
            index {
                id
            }
        }
    }
`;

export const getIndexUnitsUpdatedEvents = gql`
    query getIndexUnitsUpdatedEvents($transactionHash: Bytes!) {
        response: indexUnitsUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            units
            subscriber
            userData
            index {
                id
            }
        }
    }
`;

export const getIndexUnsubscribedEvents = gql`
    query getIndexUnsubscribedEvents($transactionHash: Bytes!) {
        response: indexUnsubscribedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            subscriber
            userData
            index {
                id
            }
        }
    }
`;

export const getSubscriptionApprovedEvents = gql`
    query getSubscriptionApprovedEvents($transactionHash: Bytes!) {
        response: subscriptionApprovedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            subscriber
            subscription {
                id
            }
            publisher
            indexId
            userData
        }
    }
`;

export const getSubscriptionRevokedEvents = gql`
    query getSubscriptionRevokedEvents($transactionHash: Bytes!) {
        response: subscriptionRevokedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            subscriber
            subscription {
                id
            }
            publisher
            indexId
            userData
        }
    }
`;

export const getSubscriptionUnitsUpdatedEvents = gql`
    query getSubscriptionUnitsUpdatedEvents($transactionHash: Bytes!) {
        response: subscriptionUnitsUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
            subscriber
            subscription {
                id
            }
            publisher
            indexId
            units
            userData
        }
    }
`;
