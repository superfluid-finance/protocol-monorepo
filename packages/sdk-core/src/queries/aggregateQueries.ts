import { gql } from "graphql-request";

export const getAccountTokenSnapshotsByAccount = gql`
    query getAccountTokenSnapshotsByAccount($account: ID!) {
        response: accountTokenSnapshots(where: { account: $account }) {
            id
            updatedAtTimestamp
            updatedAtBlockNumber
            totalNumberOfActiveStreams
            totalNumberOfClosedStreams
            totalSubscriptionsWithUnits
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
                createdAtTimestamp
                createdAtBlockNumber
                name
                symbol
                isListed
                underlyingAddress
            }
        }
    }
`;
