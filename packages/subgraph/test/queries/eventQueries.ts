import { gql } from "graphql-request";

// CFA Event Queries
export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents($transactionHash: Bytes!) {
        flowUpdateds(where: { transactionHash: $transactionHash }) {
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
        indexCreateds(where: { transactionHash: $transactionHash }) {
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
        indexUpdateds(where: { transactionHash: $transactionHash }) {
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
        subscriptionApproveds(where: { transactionHash: $transactionHash }) {
            transactionHash
            blockNumber
            token
            subscriber {
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
        subscriptionRevokeds(where: { transactionHash: $transactionHash }) {
            transactionHash
            blockNumber
            token
            subscriber {
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
        subscriptionUnitsUpdateds(
            where: { transactionHash: $transactionHash }
        ) {
            transactionHash
            blockNumber
            token
            subscriber {
                id
            }
            publisher
            indexId
            units
            userData
        }
    }
`;
