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
                balanceSoFar
                logIndex
                order
                timestamp
                totalAmountStreamedSoFar
                totalAmountTransferredSoFar
                maybeCriticalAtTimestampSoFar
                totalApprovedSubscriptionsSoFar
                totalDepositSoFar
                totalInflowRateSoFar
                totalNetFlowRateSoFar
                totalNumberOfActiveStreamsSoFar
                totalNumberOfClosedStreamsSoFar
                totalOutflowRateSoFar
                totalSubscriptionsWithUnitsSoFar
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
        }
    }
`;
