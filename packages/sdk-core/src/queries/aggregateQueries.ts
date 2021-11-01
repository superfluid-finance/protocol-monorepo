import { gql } from "graphql-request";
import { IPaginateResponse } from "../interfaces";

export const getAccountTokenSnapshotsQuery = (
    where: string,
    paginateOptions: IPaginateResponse
) => gql`
    query getAccountTokenSnapshotsByAccount($account: ID!) {
        response: accountTokenSnapshots(
            where: { ${where} },
			first: ${paginateOptions.first}, 
			skip: ${paginateOptions.skip}
        ) {
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
