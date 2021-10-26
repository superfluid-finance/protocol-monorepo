import { gql } from "graphql-request";
import { IPaginateResponse } from "../interfaces";
import { baseHOLProperties, baseUpdateableProperties } from "./baseProperties";

export const getSuperTokensQuery = gql`
    {
        response: tokens(where: { isSuperToken: true }) {
            ${baseHOLProperties}
            name
            symbol
            isListed
            underlyingAddress
        }
    }
`;

export const getStreamsQuery = (
    where: string,
    paginateOptions: IPaginateResponse
) => gql`
    {
        response: streams(
			where: { ${where} },
			first: ${paginateOptions.first},
			skip: ${paginateOptions.skip}
        ) {
            ${baseUpdateableProperties}
            currentFlowRate
            streamedUntilUpdatedAt
            token {
                id
                createdAtTimestamp
                createdAtBlockNumber
                name
                symbol
                isListed
                underlyingAddress
            }
            sender {
                id
            }
            receiver {
                id
            }
            flowUpdatedEvents(orderBy: timestamp, orderDirection: "asc") {
                id
                blockNumber
                timestamp
                transactionHash
                token
                sender
                receiver
                flowRate
                totalSenderFlowRate
                totalReceiverFlowRate
                userData
                oldFlowRate
                type
                totalAmountStreamedUntilTimestamp
            }
        }
    }
`;

export const getIndexesQuery = (
    where: string,
    paginateOptions: IPaginateResponse
) => gql`
	{
		response: indexes(
			where: { ${where} },
			first: ${paginateOptions.first},
			skip: ${paginateOptions.skip}
		) {
			${baseUpdateableProperties}
			indexId
			indexValue
			totalSubscriptionsWithUnits
			totalUnitsPending
			totalUnitsApproved
			totalUnits
			totalAmountDistributedUntilUpdatedAt
			token {
				id
				createdAtTimestamp
				createdAtBlockNumber
				name
				symbol
				isListed
				underlyingAddress
			}
			publisher {
				id
			}
		}
	}
`;

export const getIndexSubscriptionsQuery = (
    where: string,
    paginateOptions: IPaginateResponse
) => gql`
	{
		response: indexSubscriptions(
			where: { ${where} }, 
			first: ${paginateOptions.first}, 
			skip: ${paginateOptions.skip}
		) {
			${baseUpdateableProperties}
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
				indexValue
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
	}
`;
