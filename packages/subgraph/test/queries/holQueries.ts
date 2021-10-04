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
            userData
            oldIndexValue
            newIndexValue
            totalSubscribers
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

export const getSubscriber = gql`
    query getSubscriber($id: ID!) {
        subscriber(id: $id) {
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
            userData
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
