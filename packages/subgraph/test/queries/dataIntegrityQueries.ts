import { gql } from "graphql-request";

export const getStreams = gql`
    query getStreams($blockNumber: Int) {
        response: streams(block: { number: $blockNumber }) {
            id
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

export const getIndexes = gql`
    query getIndex($blockNumber: Int, $first: Int, $skip: Int) {
        response: indexes(
            block: { number: $blockNumber }
            first: $first
            skip: $skip
        ) {
            id
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
    query getSubscription($blockNumber: Int, $first: Int, $skip: Int) {
        response: indexSubscriptions(
            block: { number: $blockNumber }
            first: $first
            skip: $skip
        ) {
            id
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
