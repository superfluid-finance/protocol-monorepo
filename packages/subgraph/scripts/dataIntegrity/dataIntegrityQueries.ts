import { gql } from "graphql-request";

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

/**
 * Gets account token snapshots of all accounts that have ever interacted with
 * the Superfluid protocol.
 */
export const getAccountTokenSnapshots = gql`
    query getAccountTokenSnapshots($blockNumber: Int, $first: Int, $createdAt: Int) {
        response: accountTokenSnapshots(
            block: { number: $blockNumber }
            first: $first
            where: { createdAtTimestamp_gte: $createdAt }
            orderBy: createdAtTimestamp
            orderDirection: asc
        ) {
            id
            createdAtTimestamp
            totalNetFlowRate
            token {
                id
            }
            account {
                id
            }
        }
    }
`;