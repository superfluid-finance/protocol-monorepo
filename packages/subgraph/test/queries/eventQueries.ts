import { gql } from "graphql-request";

// CFA Event Queries
export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents($transactionHash: Bytes!) {
        flowUpdatedEvents(where: { transactionHash: $transactionHash }) {
            id
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
        indexCreatedEvents(where: { transactionHash: $transactionHash }) {
            id
            transactionHash
            blockNumber
            token
            publisher
            indexId
            userData
        }
    }
`;

export const getIndexUpdatedEvents = gql`
    query getIndexUpdatedEvents($transactionHash: Bytes!) {
        indexUpdatedEvents(where: { transactionHash: $transactionHash }) {
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
        }
    }
`;
export const getSubscriptionApprovedEvents = gql`
    query getSubscriptionApprovedEvents($transactionHash: Bytes!) {
        subscriptionApprovedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
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
        subscriptionRevokedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
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
        subscriptionUnitsUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            transactionHash
            blockNumber
            token
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
