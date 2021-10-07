import { gql } from "graphql-request";

export const getStream = gql`
    query getStream($id: ID!) {
        stream(id: $id) {
            id
            currentFlowRate
            streamedUntilUpdatedAt
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

export const getIndex = gql`
    query getIndex($id: ID!) {
        index(id: $id) {
            id
            indexId
            oldIndexValue
            newIndexValue
            totalSubscriptions
            totalUnitsPending
            totalUnitsApproved
            totalUnits
            totalAmountDistributedUntilUpdatedAt
            token {
                id
            }
            publisher {
                id
            }
        }
    }
`;

export const getSubscription = gql`
    query getSubscription($id: ID!) {
        indexSubscription(id: $id) {
            id
            token {
                id
            }
            subscriber {
                id
            }
            publisher {
                id
            }
            indexId
            approved
            units
            totalAmountReceivedUntilUpdatedAt
            lastIndexValue
            index {
                id
            }
        }
    }
`;
