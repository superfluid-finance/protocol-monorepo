import {
    IAccountTokenSnapshotFilter,
    IIndex,
    IIndexRequestFilter,
    IIndexSubscription,
    IIndexSubscriptionRequestFilter,
    ILightAccountTokenSnapshot,
    IStream,
    IStreamRequestFilter,
    ISuperToken,
    ISuperTokenRequestFilter,
} from "./interfaces";
import { DataMode } from "./types";
import {
    GetIndexesDocument,
    GetIndexesQuery,
    GetIndexesQueryVariables,
} from "./subgraph/queries/getIndexes.generated";
import {
    validateAccountTokenSnapshotRequest,
    validateIndexRequest,
    validateIndexSubscriptionRequest,
    validateStreamRequest,
    validateSuperTokenRequest,
} from "./validation";
import { PagedResult, Paging } from "./pagination";
import { SubgraphClient } from "./subgraph/SubgraphClient";
import {
    GetTokensDocument,
    GetTokensQuery,
    GetTokensQueryVariables,
} from "./subgraph/queries/getTokens.generated";
import {
    GetIndexSubscriptionsDocument,
    GetIndexSubscriptionsQuery,
    GetIndexSubscriptionsQueryVariables,
} from "./subgraph/queries/getIndexSubscriptions.generated";
import {
    GetStreamsDocument,
    GetStreamsQuery,
    GetStreamsQueryVariables,
} from "./subgraph/queries/getStreams.generated";
import {
    GetAccountTokenSnapshotsDocument,
    GetAccountTokenSnapshotsQuery,
    GetAccountTokenSnapshotsQueryVariables,
} from "./subgraph/queries/getAccountTokenSnapshots.generated";
import { AccountEvents, AllEvents, IEventFilter } from "./events";
import {
    GetAllEventsDocument,
    GetAllEventsQuery,
    GetAllEventsQueryVariables,
} from "./subgraph/queries/events/getAllEvents.generated";
import { mapGetAllEventsQueryEvents } from "./mapGetAllEventsQueryEvents";

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
        this.subgraphClient = new SubgraphClient(
            this.options.customSubgraphQueriesEndpoint
        );
    }

    listAllSuperTokens = async (
        filter: ISuperTokenRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<ISuperToken>> => {
        validateSuperTokenRequest(filter);

        const response = await this.subgraphClient.request<
            GetTokensQuery,
            GetTokensQueryVariables
        >(GetTokensDocument, {
            where: {
                isSuperToken: true,
                ...filter,
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<ISuperToken>(response.result, paging);
    };

    listIndexes = async (
        filter: IIndexRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndex>> => {
        validateIndexRequest(filter);

        const response = await this.subgraphClient.request<
            GetIndexesQuery,
            GetIndexesQueryVariables
        >(GetIndexesDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<IIndex>(response.result, paging);
    };

    listIndexSubscriptions = async (
        filter: IIndexSubscriptionRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndexSubscription>> => {
        validateIndexSubscriptionRequest(filter);

        const response = await this.subgraphClient.request<
            GetIndexSubscriptionsQuery,
            GetIndexSubscriptionsQueryVariables
        >(GetIndexSubscriptionsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<IIndexSubscription>(response.result, paging);
    };

    listStreams = async (
        filter: IStreamRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IStream>> => {
        validateStreamRequest(filter);

        const response = await this.subgraphClient.request<
            GetStreamsQuery,
            GetStreamsQueryVariables
        >(GetStreamsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<IStream>(response.result, paging);
    };

    listUserInteractedSuperTokens = async (
        filter: IAccountTokenSnapshotFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<ILightAccountTokenSnapshot>> => {
        validateAccountTokenSnapshotRequest(filter);

        const response = await this.subgraphClient.request<
            GetAccountTokenSnapshotsQuery,
            GetAccountTokenSnapshotsQueryVariables
        >(GetAccountTokenSnapshotsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<ILightAccountTokenSnapshot>(
            response.result,
            paging
        );
    };

    listEvents = async (
        filter: IEventFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<AllEvents>> => {
        const response = await this.subgraphClient.request<
            GetAllEventsQuery,
            GetAllEventsQueryVariables
        >(GetAllEventsDocument, {
            where: {
                addresses_contains: filter.account
                    ? [filter.account]
                    : undefined,
                timestamp_gte: filter.timestamp_gte,
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });
        return new PagedResult<AllEvents>(
            mapGetAllEventsQueryEvents(response),
            paging
        );
    };

    on(
        callback: (events: AccountEvents[], unsubscribe: () => void) => void,
        ms: number,
        account: string,
        timeout?: number
    ): () => void {
        console.log("on");
        if (ms < 1000) throw Error("Let's not go crazy with the queries...");

        // TODO: Wait for answer before next query...

        const timeSkew = 25000;

        let nextUtcNow = new Date().getTime() - timeSkew;
        const intervalId = setInterval(async () => {
            console.log("interval");
            const utcNow = nextUtcNow;
            nextUtcNow += ms;

            const subgraphTime = Math.floor(utcNow / 1000);
            const accountEvents: AccountEvents[] = await this.listEvents({
                account: account,
                timestamp_gte: subgraphTime.toString(),
            }).then((x) => x.data as AccountEvents[]); // TODO(KK): Any way to do it without unsafe cast?

            if (accountEvents.length) {
                console.log("callback");
                callback(accountEvents, unsubscribe);
            }
        }, ms);

        const unsubscribe = () => {
            console.log("unsubscribe");
            clearInterval(intervalId);
        };

        if (timeout) {
            setTimeout(() => {
                console.log("timeout");
                unsubscribe();
            }, timeout);
        }

        return unsubscribe;
    }
}
