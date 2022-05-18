import { gql } from "graphql-request";

export const getAccountTokenSnapshot = gql`
    query getAccountTokenSnapshot($id: ID!) {
        response: accountTokenSnapshot(id: $id) {
            updatedAtTimestamp
            updatedAtBlockNumber
            totalNumberOfActiveStreams
            maybeCriticalAtTimestamp
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
                totalApprovedSubscriptionsSoFar
                totalDepositSoFar
                totalInflowRateSoFar
                totalNetFlowRateSoFar
                totalNumberOfActiveStreamsSoFar
                totalNumberOfClosedStreamsSoFar
                totalOutflowRateSoFar
                totalSubscriptionsWithUnitsSoFar
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

export const getAccountTokenSnapshotLogs = gql`
    query getAccountTokenSnapshotLogs($id: ID!) {
        response: accountTokenSnapshots(
            where: { account: $id }
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
            totalApprovedSubscriptionsSoFar
            totalDepositSoFar
            totalInflowRateSoFar
            totalNetFlowRateSoFar
            totalNumberOfActiveStreamsSoFar
            totalNumberOfClosedStreamsSoFar
            totalOutflowRateSoFar
            totalSubscriptionsWithUnitsSoFar
            account {
                id
            }
            token {
                id
            }
        }
    }
`;
