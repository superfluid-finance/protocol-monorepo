import {gql} from "graphql-request";

export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents(
        $blockNumber: Int
        $first: Int
        $timestamp: Int
    ) {
        response: flowUpdatedEvents(
            block: {number: $blockNumber}
            first: $first
            where: {timestamp_gte: $timestamp}
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

/**
 * Gets current streams (where flow rate > 0)
 */
export const getCurrentStreams = gql`
    query getStreams($blockNumber: Int, $first: Int, $createdAt: Int) {
        response: streams(
            block: {number: $blockNumber}
            first: $first
            where: {currentFlowRate_gt: 0, createdAtTimestamp_gte: $createdAt}
            orderBy: createdAtTimestamp
            orderDirection: asc
        ) {
            id
            createdAtTimestamp
            updatedAtTimestamp
            currentFlowRate
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
            block: {number: $blockNumber}
            first: $first
            where: {createdAtTimestamp_gte: $createdAt}
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
            block: {number: $blockNumber}
            first: $first
            where: {createdAtTimestamp_gte: $createdAt}
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
            block: {number: $blockNumber}
            first: $first
            where: {updatedAtTimestamp_gte: $updatedAt}
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
            block: {number: $blockNumber}
            first: $first
            where: {updatedAtTimestamp_gte: $updatedAt}
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
