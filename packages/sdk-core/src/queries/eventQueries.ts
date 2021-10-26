import { gql } from "graphql-request";

export const getAllEvents = () => gql`
	query getAllEvents() {
		response: events() {
			id
			blockNumber
			timestamp
			transactionHash
			__typename
		}
	}
`;

export const getEventByType = () => gql`
	query getEvent() {
		response: ${"eventTypeToName"}() {

		}
	}
`;
