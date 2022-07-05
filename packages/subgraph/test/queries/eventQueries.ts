import {gql} from "graphql-request";

// CFA Event Queries
export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents($transactionHash: Bytes!) {
        response: flowUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            timestamp
            name
            blockNumber
            logIndex
            addresses
            token
            sender
            receiver
            flowOperator
            flowRate
            totalAmountStreamedUntilTimestamp
            totalSenderFlowRate
            totalReceiverFlowRate
            deposit
            oldFlowRate
            type
        }
    }
`;

export const getFlowOperatorUpdatedEvents = gql`
    query getFlowOperatorUpdatedEvents($transactionHash: Bytes!) {
        response: flowOperatorUpdatedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            timestamp
            name
            blockNumber
            logIndex
            addresses
            token
            sender
            flowOperator
            permissions
            flowRateAllowance
            flowOperator {
                id
            }
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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

export const getIndexDistributionClaimedEvents = gql`
    query getIndexDistributionClaimedEvents($transactionHash: Bytes!) {
        response: indexDistributionClaimedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
            token
            publisher
            indexId
            subscriber
            amount
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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

export const getSubscriptionDistributionClaimedEvents = gql`
    query getSubscriptionDistributionClaimedEvents($transactionHash: Bytes!) {
        response: subscriptionDistributionClaimedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
            token
            subscriber
            publisher
            indexId
            amount
            subscription {
                id
            }
        }
    }
`;

export const getSubscriptionRevokedEvents = gql`
    query getSubscriptionRevokedEvents($transactionHash: Bytes!) {
        response: subscriptionRevokedEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            addresses
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

export const getTransferEvents = gql`
    query getTransferEvents($transactionHash: Bytes!) {
        response: transferEvents(
            where: { transactionHash: $transactionHash }
        ) {
            id
            order
            transactionHash
            gasPrice
            name
            blockNumber
            logIndex
            from
            to
            value
        }
    }
`;