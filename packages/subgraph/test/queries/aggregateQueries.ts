import { gql } from "graphql-request";

export const getAccountTokenSnapshot = gql`
    query getAccountTokenSnapshot($id: ID!) {
        accountTokenSnapshot(id: $id) {
            updatedAtTimestamp
            totalNumberOfActiveStreams
            totalNumberOfClosedStreams
            totalSubscriptions
            totalApprovedSubscriptions
            balanceUntilUpdatedAt
            totalNetFlowRate
            totalInflowRate
            totalOutflowRate
            totalAmountStreamedUntilUpdatedAt
            totalAmountTransferredUntilUpdatedAt
            account {
                id
            }
            token {
                id
            }
        }
    }
`;

export const getTokenStatistic = gql`
    query getTokenStatistic($id: ID!) {
        tokenStatistic(id: $id) {
            id
            updatedAtTimestamp
            totalNumberOfActiveStreams
            totalNumberOfClosedStreams
            totalNumberOfIndexes
            totalNumberOfActiveIndexes
            totalSubscriptions
            totalApprovedSubscriptions
            totalOutflowRate
            totalAmountStreamedUntilUpdatedAt
            totalAmountTransferredUntilUpdatedAt
            totalAmountDistributedUntilUpdatedAt
            token {
                id
            }
        }
    }
`;
