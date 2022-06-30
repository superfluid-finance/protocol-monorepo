import { gql } from "graphql-request";

export const getAccountTokenSnapshot = gql`
    query getAccountTokenSnapshot($id: ID!) {
        response: accountTokenSnapshot(id: $id) {
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
            totalAmountStreamedInUntilUpdatedAt
            totalAmountStreamedOutUntilUpdatedAt
            totalAmountTransferredUntilUpdatedAt
            totalDeposit
            maybeCriticalAtTimestamp
            flowOperators(orderBy: createdAtTimestamp, orderDirection: asc) {
                id
            }
            account {
                id
            }
            token {
                id
            }
            accountTokenSnapshotLogs(
                first: 1
                orderBy: order
                orderDirection: desc
            ) {
                blockNumber
                transactionHash
                balance
                logIndex
                order
                timestamp
                totalAmountStreamed
                totalAmountStreamedIn
                totalAmountStreamedOut
                totalAmountTransferred
                maybeCriticalAtTimestamp
                totalApprovedSubscriptions
                totalDeposit
                totalInflowRate
                totalNetFlowRate
                totalNumberOfActiveStreams
                totalNumberOfClosedStreams
                totalOutflowRate
                totalSubscriptionsWithUnits
                triggeredByEventName
            }
        }
    }
`;

export const getTokenStatistic = gql`
    query getTokenStatistic($id: ID!) {
        response: tokenStatistic(id: $id) {
            id
            updatedAtTimestamp
            updatedAtBlockNumber
            totalNumberOfActiveStreams
            totalNumberOfClosedStreams
            totalNumberOfIndexes
            totalNumberOfActiveIndexes
            totalSubscriptionsWithUnits
            totalApprovedSubscriptions
            totalOutflowRate
            totalDeposit
            totalSupply
            totalAmountStreamedUntilUpdatedAt
            totalAmountTransferredUntilUpdatedAt
            totalAmountDistributedUntilUpdatedAt
            token {
                id
            }
            tokenStatisticLogs(first: 1, orderBy: order, orderDirection: desc) {
                timestamp
                blockNumber
                transactionHash
                logIndex
                order
                triggeredByEventName
                totalNumberOfActiveStreams
                totalNumberOfClosedStreams
                totalNumberOfIndexes
                totalNumberOfActiveIndexes
                totalSubscriptionsWithUnits
                totalApprovedSubscriptions
                totalDeposit
                totalOutflowRate
                totalAmountStreamed
                totalAmountTransferred
                totalAmountDistributed
                totalSupply
            }
        }
    }
`;
