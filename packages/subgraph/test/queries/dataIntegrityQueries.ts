import { gql } from "graphql-request";

export const getStreams = gql`
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
                accountTokenSnapshots {
                    totalNetFlowRate
                    token {
                        id
                    }
                    account {
                        id
                    }
                }
            }
            receiver {
                id
                accountTokenSnapshots {
                    totalNetFlowRate
                    token {
                        id
                    }
                    account {
                        id
                    }
                }
            }
        }
    }
`;

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
