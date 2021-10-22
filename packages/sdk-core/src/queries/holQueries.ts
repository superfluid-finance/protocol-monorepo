import { gql } from "graphql-request";
import { IPaginate } from "../interfaces";
import { baseHOLProperties, baseUpdateableProperties } from "./baseProperties";

export const getSuperTokens = gql`
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

export const getStreams = (where: string, paginateOptions: IPaginate) => gql`
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
