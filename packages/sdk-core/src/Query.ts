import {
    DataMode,
    IAccountTokenSnapshotFilter, IEventEntityBase, IFlowUpdatedEvent,
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

export interface IQueryOptions {
    readonly customSubgraphQueriesEndpoint: string;
    readonly dataMode: DataMode;
}

/**
 * @dev Query Helper Class
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
    on(callback: (events: IEventEntityBase[]) => void, ms: number, timeout?: number, filter?: FlowUpdatedEvent_Filter) {
        console.log("on")
        let nextNow = _.now();
        const intervalId = setInterval(async () => {
            console.log("interval")
            const now = nextNow;
            nextNow += ms;

            const pagedResult = await this.listFlowUpdatedEvents({
                ...filter,
                timestamp_gte: now.toString()
            })

            if (pagedResult.data.length) {
                console.log({
                    data: pagedResult.data
                })
                callback(pagedResult.data);
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


