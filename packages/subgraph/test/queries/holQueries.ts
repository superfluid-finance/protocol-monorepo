import { gql } from "graphql-request";

export const getStream = gql`
    query getStream($id: ID!) {
        stream(where: { sender: $sender, receiver: $receiver })
        id
        currentFlowRate
        streamedUntilLastUpdatedAt
        token {
            id
        }
        sender {
            id
        }
        receiver {
            id
        }
    }
`;

export const getStreams = gql`
    query getStreams($id: ID!) {
        streams(where: { sender: $sender, receiver: $receiver })
        id
        currentFlowRate
        streamedUntilLastUpdatedAt
        token {
            id
        }
        sender {
            id
        }
        receiver {
            id
        }
    }
`;
