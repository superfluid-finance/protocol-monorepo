import { gql } from "graphql-request";

export const getSuperTokens = gql`
    {
        response: tokens(where: { isSuperToken: true }) {
            id
            createdAtTimestamp
            createdAtBlockNumber
            name
            symbol
            isListed
            underlyingAddress
        }
    }
`;

export const getStreams = gql`
    query getStreams($sender: ID!, $receiver: ID!, $token: ID) {
        response: streams(
            where: { sender: $sender, receiver: $receiver, token: $token }
        ) {
            id
            createdAtTimestamp
            createdAtBlockNumber
            updatedAtTimestamp
            updatedAtBlockNumber
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
