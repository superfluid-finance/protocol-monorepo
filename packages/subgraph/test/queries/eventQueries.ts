import { gql } from "graphql-request";
import { formatQueryOptions } from "../helpers/helpers";
import { IQueryOptions } from "../interfaces";

const flowUpdatedEventProperties = `{
	id
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
}`;

export const getFlowUpdatedEventQuery = gql`
query getFlowUpdatedEvent($id: ID!) {
	flowUpdated(id: $id) {
		${flowUpdatedEventProperties}
	}
}`;

export const getFlowUpdatedEventsQuery = (options: IQueryOptions) => gql`
    query getFlowUpdatedEvents($sender: Bytes!, $receiver: Bytes!) {
        flowUpdateds(where: { sender: $sender, receiver: $receiver } ${formatQueryOptions(
            options
        )} ) ${flowUpdatedEventProperties}
    }
`;
