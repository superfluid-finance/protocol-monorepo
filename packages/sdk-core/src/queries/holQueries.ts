import { gql } from "graphql-request";
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

export const getStreams = (where: string) => gql`
    {
        response: streams(
            where: { ${where} }
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
