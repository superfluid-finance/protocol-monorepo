import { gql } from "graphql-request";

export const getAccount = gql`
    query getAccount($id: ID!) {
        response: account(id: $id) {
            id
            isSuperApp
            inflows(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
            outflows(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
        }
    }
`;

export const getStream = gql`
    query getStream($id: ID!) {
        response: stream(id: $id) {
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
            flowUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
        }
    }
`;

export const getIndex = gql`
    query getIndex($id: ID!) {
        response: index(id: $id) {
            id
            indexId
            indexValue
            totalSubscriptionsWithUnits
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
            subscriptions(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
            indexCreatedEvent(orderBy: timestamp, orderDirection: asc) {
                id
            }
            indexUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
        }
    }
`;

export const getSubscription = gql`
    query getSubscription($id: ID!) {
        response: indexSubscription(id: $id) {
            id
            subscriber {
                id
            }
            approved
            units
            totalAmountReceivedUntilUpdatedAt
            indexValueUntilUpdatedAt
            index {
                id
                indexId
                token {
                    id
                }
                publisher {
                    id
                }
            }
            subscriptionApprovedEvents(
                orderBy: timestamp
                orderDirection: asc
            ) {
                id
            }
            subscriptionRevokedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
            subscriptionUnitsUpdatedEvents(
                orderBy: timestamp
                orderDirection: asc
            ) {
                id
            }
        }
    }
`;
