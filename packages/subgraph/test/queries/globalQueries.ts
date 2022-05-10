import { gql } from "graphql-request";

const getAccountIds = gql`
    query getAccountIds($id: ID!) {
        response: accounts(where: { id: $id }) {
            id
        }
    }
`;

const getAccountTokenSnapshotIds = gql`
    query getAccountTokenSnapshotIds($id: ID!) {
        response: accountTokenSnapshots(where: { account: $id }) {
            id
        }
    }
`;

const getEvents = gql`
    query getEvents {
        response: events {
            id
        }
    }
`;

const getFlowUpdatedEventsLightEntities = gql`
    query getFlowUpdatedEventsLightEntities {
        response: flowUpdatedEvents {
            stream {
                id
            }
        }
    }
`;

const getFlowOperatorUpdatedEventsLightEntities = gql`
    query getFlowOperatorUpdatedEventsLightEntities {
        response: flowOperatorUpdatedEvents {
            flowOperator {
                id
            }
        }
    }
`;

const getIndexCreatedEventsLightEntities = gql`
    query getIndexCreatedEventsLightEntities {
        response: indexCreatedEvents {
            index {
                id
            }
        }
    }
`;

const getIndexDistributionClaimedEventsLightEntities = gql`
    query getIndexDistributionClaimedEventsLightEntities {
        response: indexDistributionClaimedEvents {
            index {
                id
            }
        }
    }
`;

const getIndexUpdatedEventsLightEntities = gql`
    query getIndexUpdatedEventsLightEntities {
        response: indexUpdatedEvents {
            index {
                id
            }
        }
    }
`;

const getIndexSubscribedEventsLightEntities = gql`
    query getIndexSubscribedEventsLightEntities {
        response: indexSubscribedEvents {
            index {
                id
            }
        }
    }
`;

const getIndexUnitsUpdatedEventsLightEntities = gql`
    query getIndexUnitsUpdatedEventsLightEntities {
        response: indexUnitsUpdatedEvents {
            index {
                id
            }
        }
    }
`;

const getIndexUnsubscribedEventsLightEntities = gql`
    query getIndexUnsubscribedEventsLightEntities {
        response: indexUnsubscribedEvents {
            index {
                id
            }
        }
    }
`;

const getSubscriptionApprovedEventsLightEntities = gql`
    query getSubscriptionApprovedEventsLightEntities {
        response: subscriptionApprovedEvents {
            subscription {
                id
            }
        }
    }
`;

const getSubscriptionDistributionClaimedEventsLightEntities = gql`
    query getSubscriptionDistributionClaimedEventsLightEntities {
        response: subscriptionDistributionClaimedEvents {
            subscription {
                id
            }
        }
    }
`;

const getSubscriptionRevokedEventsLightEntities = gql`
    query getSubscriptionRevokedEventsLightEntities {
        response: subscriptionRevokedEvents {
            subscription {
                id
            }
        }
    }
`;

const getSubscriptionUnitsUpdatedEventsLightEntities = gql`
    query getSubscriptionUnitsUpdatedEventsLightEntities {
        response: subscriptionUnitsUpdatedEvents {
            subscription {
                id
            }
        }
    }
`;

const getTransferEventsLightEntities = gql`
    query getTransferEventsLightEntities {
        response: transferEvents {
            from {
                id
            }
            to {
                id
            }
        }
    }
`;

const getTokenDowngradedEventsLightEntities = gql`
    query getTokenDowngradedEventsLightEntities {
        response: tokenDowngradedEvents {
            account {
                id
            }
        }
    }
`;

const getTokenUpgradedEventsLightEntities = gql`
    query getTokenUpgradedEventsLightEntities {
        response: tokenUpgradedEvents {
            account {
                id
            }
        }
    }
`;

const getAccountsLightEntities = gql`
    query getAccountsLightEntities {
        response: accounts {
            inflows(first: 1) {
                id
            }
            outflows(first: 1) {
                id
            }
            subscriptions(first: 1) {
                id
            }
            publishedIndexes(first: 1) {
                id
            }
            sentTransferEvents(first: 1) {
                id
            }
            receivedTransferEvents(first: 1) {
                id
            }
            tokenUpgradedEvents(first: 1) {
                id
            }
            tokenDowngradedEvents(first: 1) {
                id
            }
            accountTokenSnapshots(first: 1) {
                id
            }
        }
    }
`;

const getIndexesLightEntities = gql`
    query getIndexLightEntities {
        response: indexes {
            token {
                id
            }
            publisher {
                id
            }
            subscriptions(first: 1) {
                id
            }
            indexCreatedEvent {
                id
            }
            indexDistributionClaimedEvents(first: 1) {
                id
            }
            indexUpdatedEvents(first: 1) {
                id
            }
            indexSubscribedEvents(first: 1) {
                id
            }
            indexUnitsUpdatedEvents(first: 1) {
                id
            }
            indexUnsubscribedEvents(first: 1) {
                id
            }
        }
    }
`;

const getIndexSubscriptionsLightEntities = gql`
    query getIndexSubscriptionsLightEntities {
        response: indexSubscriptions {
            subscriber {
                id
            }
            index {
                id
            }
            subscriptionApprovedEvents(first: 1) {
                id
            }
            subscriptionDistributionClaimedEvents(first: 1) {
                id
            }
            subscriptionRevokedEvents(first: 1) {
                id
            }
            subscriptionUnitsUpdatedEvents(first: 1) {
                id
            }
        }
    }
`;

const getStreamsLightEntities = gql`
    query getStreamsLightEntities {
        response: streams {
            token {
                id
            }
            sender {
                id
            }
            receiver {
                id
            }
            flowUpdatedEvents(first: 1) {
                id
            }
            streamPeriods(first: 1) {
                id
            }
        }
    }
`;

const getFlowOperatorsLightEntities = gql`
    query getFlowOperatorsLightEntities {
        response: flowOperators {
            token {
                id
            }
            sender {
                id
            }
            flowOperatorUpdatedEvents(first: 1) {
                id
            }
        }
    }
`;

const getStreamPeriodsLightEntities = gql`
    query getStreamPeriodsLightEntities {
        response: streamPeriods {
            stream {
                id
            }
            sender {
                id
            }
            receiver {
                id
            }
            token {
                id
            }
            startedAtEvent {
                id
            }
        }
    }
`;

const getAccountTokenSnapshotsLightEntities = gql`
    query getAccountTokenSnapshotsLightEntities {
        response: accountTokenSnapshots {
            account {
                id
            }
            token {
                id
            }
            flowOperators {
                id
            }
        }
    }
`;

const getTokenStatisticsLightEntities = gql`
    query getTokenStatisticsLightEntities {
        response: tokenStatistics {
            token {
                id
            }
        }
    }
`;

export const globalQueries = {
    getAccountIds,
    getAccountTokenSnapshotIds,
    getEvents,
    getFlowUpdatedEventsLightEntities,
    getFlowOperatorUpdatedEventsLightEntities,
    getIndexCreatedEventsLightEntities,
    getIndexDistributionClaimedEventsLightEntities,
    getIndexUpdatedEventsLightEntities,
    getIndexSubscribedEventsLightEntities,
    getIndexUnitsUpdatedEventsLightEntities,
    getIndexUnsubscribedEventsLightEntities,
    getSubscriptionApprovedEventsLightEntities,
    getSubscriptionDistributionClaimedEventsLightEntities,
    getSubscriptionRevokedEventsLightEntities,
    getSubscriptionUnitsUpdatedEventsLightEntities,
    getTransferEventsLightEntities,
    getTokenDowngradedEventsLightEntities,
    getTokenUpgradedEventsLightEntities,
    getAccountsLightEntities,
    getIndexesLightEntities,
    getIndexSubscriptionsLightEntities,
    getStreamsLightEntities,
    getFlowOperatorsLightEntities,
    getStreamPeriodsLightEntities,
    getAccountTokenSnapshotsLightEntities,
    getTokenStatisticsLightEntities,
};
