import { gql } from "graphql-request";
import { IEvent } from "../interfaces";

/**
 * Find single event based on txn hash
 * @param events
 * @param txnHash
 * @returns event entity of type T
 */
export const getSingleEvent = <T>(
    events: IEvent[],
    txnHash: string
): T | null => {
    let event = events.find((x) => x.transactionHash === txnHash);
    return event ? (event as unknown as T) : null;
};

export const getFlowUpdatedEvents = gql`
    query getFlowUpdatedEvents($sender: Bytes!, $receiver: Bytes!) {
        flowUpdateds(where: { sender: $sender, receiver: $receiver }) {
            transactionHash
            blockNumber
            token
            sender
            receiver
            flowRate
            totalSenderFlowRate
            totalReceiverFlowRate
            oldFlowRate
            type
        }
    }
`;
