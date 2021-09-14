import { gql } from "graphql-request";

export const getStreamEventsQuery = gql`
    query getStreamEvents($sender: Bytes!, $receiver: Bytes!) {
        flowUpdateds(where: { sender: $sender, receiver: $receiver }) {
            id
            sender
            receiver
            flowRate
            totalSenderFlowRate
            totalReceiverFlowRate
        }
    }
`;
