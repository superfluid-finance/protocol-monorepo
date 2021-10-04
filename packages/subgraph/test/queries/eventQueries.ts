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
        }
    }
`;
