import { gql } from "graphql-request";
import { formatQueryOptions } from "../helpers/helpers";
import { IQueryOptions } from "../interfaces";

const streamQuery = `{
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
}`;

export const getStreamQuery = gql`
query getStream($id: ID!) {
	stream(id: $id) 
	${streamQuery}
}`;

export const getStreamsQuery = (options: IQueryOptions) => gql`
query getStreams($sender: Bytes!, $receiver: Bytes!) {
	streams(where: { sender: $sender, receiver: $receiver }
	${formatQueryOptions(options)})
	${streamQuery}
}`;
