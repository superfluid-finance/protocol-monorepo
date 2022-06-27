import { gql } from "graphql-request";
import { CFAEvent, IDAEvent } from "./interfaces";

// Event Queries
export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: flowUpdatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            sender
            receiver
            flowRate
            totalSenderFlowRate
            totalReceiverFlowRate
            userData
        }
    }
`;
export const getFlowOperatorUpdatedEvents = gql`
    query getFlowOperatorUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: flowOperatorUpdatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            sender
            flowOperator
            permissions
            flowRateAllowance
        }
    }
`;

export const getIndexCreatedEvents = gql`
    query getIndexCreatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexCreatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            publisher
            indexId
            userData
        }
    }
`;

export const getIndexDistributionClaimedEvents = gql`
    query getIndexDistributionClaimedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexDistributionClaimedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            publisher
            indexId
            subscriber
            amount
        }
    }
`;

export const getIndexUpdatedEvents = gql`
    query getIndexUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexUpdatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
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

export const getIndexSubscribedEvents = gql`
    query getIndexSubscribedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexSubscribedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            publisher
            indexId
            subscriber
            userData
        }
    }
`;

export const getIndexUnitsUpdatedEvents = gql`
    query getIndexUnitsUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexUnitsUpdatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            publisher
            indexId
            subscriber
            units
            userData
        }
    }
`;

export const getIndexUnsubscribedEvents = gql`
    query getIndexUnsubscribedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: indexUnsubscribedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            publisher
            indexId
            subscriber
            userData
        }
    }
`;

export const getSubscriptionApprovedEvents = gql`
    query getSubscriptionApprovedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: subscriptionApprovedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            subscriber
            publisher
            indexId
            userData
        }
    }
`;

export const getSubscriptionDistributionClaimedEvents = gql`
    query getSubscriptionDistributionClaimedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: subscriptionDistributionClaimedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            subscriber
            publisher
            indexId
            amount
        }
    }
`;

export const getSubscriptionRevokedEvents = gql`
    query getSubscriptionRevokedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: subscriptionRevokedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            subscriber
            publisher
            indexId
            userData
        }
    }
`;

export const getSubscriptionUnitsUpdatedEvents = gql`
    query getSubscriptionUnitsUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: subscriptionUnitsUpdatedEvents(
            block: { number: $blockNumber }
            first: $first
            where: { timestamp_gte: $timestamp }
            orderBy: timestamp
            orderDirection: asc
        ) {
            id
            transactionHash
            timestamp
            token
            subscriber
            publisher
            indexId
            units
            userData
        }
    }
`;

// Higher Order Entity Queries
/**
 * Gets current streams (where flow rate > 0)
 */
export const getCurrentStreams = gql`
    query getStreams($blockNumber: Int, $first: Int, $createdAt: Int) {
        response: streams(
            block: { number: $blockNumber }
            first: $first
            where: { currentFlowRate_gt: 0, createdAtTimestamp_gte: $createdAt }
            orderBy: createdAtTimestamp
            orderDirection: asc
        ) {
            id
            createdAtTimestamp
            updatedAtTimestamp
            currentFlowRate
            deposit
            token {
                id
            }
            sender {
                id
            }
            receiver {
                id
            }
        }
    }
`;

/**
 * Gets indexes with same properties that exist when querying the contract.
 */
export const getIndexes = gql`
    query getIndex($blockNumber: Int, $first: Int, $createdAt: Int) {
        response: indexes(
            block: { number: $blockNumber }
            first: $first
            where: { createdAtTimestamp_gte: $createdAt }
            orderBy: createdAtTimestamp
            orderDirection: asc
        ) {
            id
            createdAtTimestamp
            indexId
            indexValue
            totalUnitsPending
            totalUnitsApproved
            totalUnits
            token {
                id
            }
            publisher {
                id
            }
        }
    }
`;

/**
 * Gets subscriptions with same properties that exist when querying the contract.
 */
export const getSubscriptions = gql`
    query getSubscription($blockNumber: Int, $first: Int, $createdAt: Int) {
        response: indexSubscriptions(
            block: { number: $blockNumber }
            first: $first
            where: { createdAtTimestamp_gte: $createdAt }
            orderBy: createdAtTimestamp
            orderDirection: asc
        ) {
            id
            createdAtTimestamp
            subscriber {
                id
            }
            approved
            units
            indexValueUntilUpdatedAt
            index {
                id
                indexId
                indexValue
                token {
                    id
                }
                publisher {
                    id
                }
            }
        }
    }
`;

// Aggregate Entity Queries
/**
 * Gets account token snapshots of all accounts that have ever interacted with
 * the Superfluid protocol.
 */
export const getAccountTokenSnapshots = gql`
    query getAccountTokenSnapshots(
        $blockNumber: Int
        $first: Int
        $updatedAt: Int
    ) {
        response: accountTokenSnapshots(
            block: { number: $blockNumber }
            first: $first
            where: {
                updatedAtTimestamp_gte: $updatedAt
                account_not_contains: "0x0000000000000000000000000000000000000000"
            }
            orderBy: updatedAtTimestamp
            orderDirection: asc
        ) {
            id
            updatedAtTimestamp
            balanceUntilUpdatedAt
            totalNetFlowRate
            token {
                id
                underlyingAddress
            }
            account {
                id
            }
        }
    }
`;

/**
 * Gets account token snapshots of all accounts that have ever interacted with
 * the Superfluid protocol.
 */
export const getTokenStatistics = gql`
    query getTokenStatistics($blockNumber: Int, $first: Int, $updatedAt: Int) {
        response: tokenStatistics(
            block: { number: $blockNumber }
            first: $first
            where: { updatedAtTimestamp_gte: $updatedAt }
            orderBy: updatedAtTimestamp
            orderDirection: asc
        ) {
            id
            updatedAtTimestamp
            totalSupply
            token {
                id
                underlyingAddress
            }
        }
    }
`;

export const cfaEventToQueryMap = new Map([
    [CFAEvent.FlowOperatorUpdated, getFlowOperatorUpdatedEvents],
    [CFAEvent.FlowUpdated, getFlowUpdatedEvents],
]);

export const idaEventToQueryMap = new Map([
    [IDAEvent.IndexCreated, getIndexCreatedEvents],
    [IDAEvent.IndexUpdated, getIndexUpdatedEvents],
    [IDAEvent.IndexDistributionClaimed, getIndexDistributionClaimedEvents],
    [IDAEvent.IndexSubscribed, getIndexSubscribedEvents],
    [IDAEvent.IndexUnitsUpdated, getIndexUnitsUpdatedEvents],
    [IDAEvent.IndexUnsubscribed, getIndexUnsubscribedEvents],
    [IDAEvent.SubscriptionApproved, getSubscriptionApprovedEvents],
    [
        IDAEvent.SubscriptionDistributionClaimed,
        getSubscriptionDistributionClaimedEvents,
    ],
    [IDAEvent.SubscriptionRevoked, getSubscriptionRevokedEvents],
    [IDAEvent.SubscriptionUnitsUpdated, getSubscriptionUnitsUpdatedEvents],
]);
