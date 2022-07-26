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
            subscriptions(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
            publishedIndexes(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
        }
    }
`;

export const getToken = gql`
    query getToken($id: ID!) {
        response: token(id: $id) {
            id
            name
            symbol
            decimals
            underlyingAddress
            isListed
            isNativeAssetSuperToken
            underlyingToken {
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
            flowUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
            streamPeriods(orderBy: startedAtTimestamp, orderDirection: asc) {
                id
            }
        }
    }
`;

export const getStreamPeriod = gql`
    query getStreamPeriod($id: ID!) {
        response: streamPeriod(id: $id) {
            id
            flowRate

            token {
                id
            }
            sender {
                id
            }
            receiver {
                id
            }
            deposit
            startedAtTimestamp
            startedAtBlockNumber
            startedAtEvent {
                id
            }

            stoppedAtTimestamp
            stoppedAtBlockNumber
            stoppedAtEvent {
                id
            }
            stream {
                id
            }

            totalAmountStreamed
        }
    }
`;

export const getFlowOperator = gql`
    query getFlowOperator($id: ID!) {
        response: flowOperator(id: $id) {
            id
            permissions
            flowRateAllowanceGranted
            flowRateAllowanceRemaining
            sender {
                id
            }
            token {
                id
            }
            accountTokenSnapshot {
                id
            }
            flowOperatorUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
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
            indexDistributionClaimedEvents(
                orderBy: timestamp
                orderDirection: asc
            ) {
                id
            }
            indexUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
            indexSubscribedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
            indexUnitsUpdatedEvents(orderBy: timestamp, orderDirection: asc) {
                id
            }
            indexUnsubscribedEvents(orderBy: timestamp, orderDirection: asc) {
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
            subscriptionDistributionClaimedEvents(
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
