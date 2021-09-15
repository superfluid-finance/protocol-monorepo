import { gql } from "graphql-request";
import { formatQueryOptions } from "../helpers/helpers";
import { IQueryOptions } from "../interfaces";

const accountTokenSnapshotProperties = `{
	id
	totalNumberOfStreams
	totalSubscriptions
	totalApprovedSubscriptions
	balance
	totalNetFlowRate
	account {
		id
	}
	token {
		id
		name
		symbol
		underlyingAddress
	}
}`;

export const getAccountTokenSnapshotQuery = gql`
	query getAccountTokenSnapshot($id: ID!) {
		accountTokenSnapshot(id: $id) {
			${accountTokenSnapshotProperties}
		}
	}
`;

export const getAccountTokenSnapshotsQuery = (options: IQueryOptions) => gql`
	query getAccountTokenSnapshots($account: Bytes!, $token: Bytes!) {
		accountTokenSnapshots(where: { account: $account, token: $token }
		${formatQueryOptions(options)}) 
		${accountTokenSnapshotProperties}
	}
`;

const tokenStatisticProperties = `{
	id
	totalNumberOfStreams
	totalNumberOfIndexes
	totalSubscribers
	totalApprovedSubscribers
	totalOutflowRate
	totalUnitsApproved
	totalUnitsPending
	totalUnitsDistributed
	token {
		id
		name
		symbol
		underlyingAddress
	}
}`;

export const getTokenStatisticQuery = gql`
	query getTokenStatistic($id: ID!) {
		tokenStatistic(id: $id) {
			${tokenStatisticProperties}
		}
	}
`;

export const getTokenStatisticsQuery = (options: IQueryOptions) => gql`
	query getTokenStatistics() {
		tokenStatistics(${formatQueryOptions(options)}) {
			${tokenStatisticProperties}
		}
	}
`;
