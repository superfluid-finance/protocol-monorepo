import {
    IAccountTokenSnapshotFilter, IFlowUpdatedEvent,
    IIndex,
    IIndexRequestFilter,
    IIndexSubscription,
    IIndexSubscriptionRequestFilter,
    ILightAccountTokenSnapshot,
    IStream,
    IStreamRequestFilter,
    ISuperToken,
    ISuperTokenRequestFilter
} from "./interfaces";
import { DataMode } from "./types";
import {GetIndexesDocument, GetIndexesQuery, GetIndexesQueryVariables} from "./subgraph/queries/getIndexes.generated";
import {
    validateAccountTokenSnapshotRequest,
    validateIndexRequest,
    validateIndexSubscriptionRequest,
    validateStreamRequest,
    validateSuperTokenRequest
} from "./validation";
import {PagedResult, Paging} from "./pagination";
import {SubgraphClient} from "./subgraph/SubgraphClient";
import {GetTokensDocument, GetTokensQuery, GetTokensQueryVariables} from "./subgraph/queries/getTokens.generated";
import {
    GetIndexSubscriptionsDocument, GetIndexSubscriptionsQuery,
    GetIndexSubscriptionsQueryVariables
} from "./subgraph/queries/getIndexSubscriptions.generated";
import {GetStreamsDocument, GetStreamsQuery, GetStreamsQueryVariables} from "./subgraph/queries/getStreams.generated";
import {
    GetAccountTokenSnapshotsDocument, GetAccountTokenSnapshotsQuery,
    GetAccountTokenSnapshotsQueryVariables
} from "./subgraph/queries/getAccountTokenSnapshots.generated";
import _ from "lodash";
import {
    GetFlowUpdatedEventsDocument,
    GetFlowUpdatedEventsQuery, GetFlowUpdatedEventsQueryVariables
} from "./subgraph/queries/getFlowUpdatedEvents.generated";
import {FlowUpdatedEvent_Filter} from "./subgraph/schema.generated";
import {
    GetAccountEventsDocument,
    GetAccountEventsQuery,
    GetAccountEventsQueryVariables
} from "./subgraph/queries/getAccountEvents.generated";
import {Events, TransferEvent} from "./events";

export interface IQueryOptions {
    readonly customSubgraphQueriesEndpoint: string;
    readonly dataMode: DataMode;
}

/**
 * @dev Query Helper Class
 * @description A helper class to create `Query` objects which can be used to query different data.
 */
export default class Query {
    options: IQueryOptions;
    private subgraphClient: SubgraphClient;

    constructor(options: IQueryOptions) {
        this.options = options;
        this.subgraphClient = new SubgraphClient(this.options.customSubgraphQueriesEndpoint)
    }

    listAllSuperTokens = async (
        filter: ISuperTokenRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<ISuperToken>> => {
        validateSuperTokenRequest(filter);

        const response = await this.subgraphClient.request<GetTokensQuery, GetTokensQueryVariables>(GetTokensDocument, {
            where: {
                isSuperToken: true,
                ...filter
            },
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<ISuperToken>(response.result, paging);
    };

    listIndexes = async (
        filter: IIndexRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndex>> => {
        validateIndexRequest(filter);

        const response = await this.subgraphClient.request<GetIndexesQuery, GetIndexesQueryVariables>(GetIndexesDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<IIndex>(response.result, paging);
    };

    listIndexSubscriptions = async (
        filter: IIndexSubscriptionRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndexSubscription>> => {
        validateIndexSubscriptionRequest(filter);

        const response = await this.subgraphClient.request<GetIndexSubscriptionsQuery, GetIndexSubscriptionsQueryVariables>(GetIndexSubscriptionsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<IIndexSubscription>(response.result, paging);
    };

    listStreams = async (
        filter: IStreamRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IStream>> => {
        validateStreamRequest(filter);

        const response = await this.subgraphClient.request<GetStreamsQuery, GetStreamsQueryVariables>(GetStreamsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<IStream>(response.result, paging);
    };

    listUserInteractedSuperTokens = async (
        filter: IAccountTokenSnapshotFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<ILightAccountTokenSnapshot>> => {
        validateAccountTokenSnapshotRequest(filter);

        const response = await this.subgraphClient.request<GetAccountTokenSnapshotsQuery, GetAccountTokenSnapshotsQueryVariables>(GetAccountTokenSnapshotsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<ILightAccountTokenSnapshot>(response.result, paging);
    };

    // TODO(KK): Don't use "FlowUpdatedEvent_Filter"
    listFlowUpdatedEvents = async (
        filter: FlowUpdatedEvent_Filter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IFlowUpdatedEvent>> => {
        const response = await this.subgraphClient.request<GetFlowUpdatedEventsQuery, GetFlowUpdatedEventsQueryVariables>(GetFlowUpdatedEventsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne()
        });

        return new PagedResult<IFlowUpdatedEvent>(response.result, paging);
    };

    // TODO(KK): Handle other events besides FlowUpdatedEvent...
    on(callback: (events: Events[]) => void, ms: number, account: string, timeout?: number) {
        console.log("on")
        if (ms < 1000)
            throw Error("Let's not go crazy with the queries...");

        // TODO: Wait for answer before next query...

        let nextNow = _.now();
        const intervalId = setInterval(async () => {
            console.log("interval")
            const now = nextNow;
            nextNow += ms;

            const response = await this.subgraphClient.request<GetAccountEventsQuery, GetAccountEventsQueryVariables>(GetAccountEventsDocument, {
                accountBytes: account,
                accountString: account,
                timestamp_gte: now.toString()
            })

            const allEvents = [
                ...response.flowUpdatedEvents_receiver,
                ...response.flowUpdatedEvents_sender,
                ...response.indexCreatedEvents,
                ...response.indexUpdatedEvents,
                ...response.indexUnitsUpdatedEvents_publisher,
                ...response.indexUnitsUpdatedEvents_subscriber,
                ...response.indexSubscribedEvents_publisher,
                ...response.indexSubscribedEvents_subscriber,
                ...response.indexUnsubscribedEvents_publisher,
                ...response.indexUnsubscribedEvents_subscriber,
                ...response.indexDistributionClaimedEvents_publisher,
                ...response.indexDistributionClaimedEvents_subscriber,
                ...response.transferEvents_to.map<TransferEvent>(x => ({
                    __typename: x.__typename,
                    token: x.token,
                    value: x.value,
                    from: x.from.id,
                    to: x.to.id
                })),
                ...response.transferEvents_from.map<TransferEvent>(x => ({
                    __typename: x.__typename,
                    token: x.token,
                    value: x.value,
                    from: x.from.id,
                    to: x.to.id
                })),
                ...response.tokenDowngradedEvents,
                ...response.tokenUpgradedEvents,
            ]

            if (allEvents.length) {
                callback(allEvents);
            }
        }, ms)

        const unsubscribe = () => {
            console.log("unsubscribe")
            clearInterval(intervalId);
        }

        if (timeout) {
            setTimeout(() => {
                console.log("timeout")
                unsubscribe();
            })
        }

        return unsubscribe;
    }
}


