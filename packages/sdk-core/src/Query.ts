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
    validateEventRequest,
    validateIndexRequest,
    validateIndexSubscriptionRequest,
    validateStreamRequest,
    validateSuperTokenRequest,
} from "./validation";
import { createPagedResult, PagedResult, Paging } from "./pagination";
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
} from "./subgraph/queries/getAllEvents.generated";
import { mapGetAllEventsQueryEvents } from "./mapGetAllEventsQueryEvents";
import SFError from "./SFError";

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
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateSuperTokenRequest(filter);

        const response = await this.subgraphClient.request<
            GetTokensQuery,
            GetTokensQueryVariables
        >(GetTokensDocument, {
            where: {
                isListed: filter.isListed,
                isSuperToken: true,
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const mappedResult = response.result.map((x) =>
            typeGuard<ISuperToken>({
                ...x,
                createdAtTimestamp: Number(x.createdAtTimestamp),
                createdAtBlockNumber: Number(x.createdAtBlockNumber),
            })
        );

        return createPagedResult<ISuperToken>(mappedResult, paging);
    };

    listIndexes = async (
        filter: IIndexRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndex>> => {
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateIndexRequest(filter);

        const response = await this.subgraphClient.request<
            GetIndexesQuery,
            GetIndexesQueryVariables
        >(GetIndexesDocument, {
            where: {
                indexId: filter.indexId,
                publisher: filter.publisher?.toLowerCase(),
                token: filter.token?.toLowerCase(),
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const mappedResult = response.result.map((x) =>
            typeGuard<IIndex>({
                ...x,
                publisher: x.publisher.id,
                createdAtTimestamp: Number(x.createdAtTimestamp),
                createdAtBlockNumber: Number(x.createdAtBlockNumber),
                updatedAtTimestamp: Number(x.updatedAtTimestamp),
                updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
                token: {
                    ...x.token,
                    createdAtTimestamp: Number(x.token.createdAtTimestamp),
                    createdAtBlockNumber: Number(x.token.createdAtBlockNumber),
                },
            })
        );

        return createPagedResult<IIndex>(mappedResult, paging);
    };

    listIndexSubscriptions = async (
        filter: IIndexSubscriptionRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IIndexSubscription>> => {
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateIndexSubscriptionRequest(filter);

        const response = await this.subgraphClient.request<
            GetIndexSubscriptionsQuery,
            GetIndexSubscriptionsQueryVariables
        >(GetIndexSubscriptionsDocument, {
            where: {
                subscriber: filter.subscriber?.toLowerCase(),
                approved: filter.approved,
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const mappedResult = response.result.map((x) =>
            typeGuard<IIndexSubscription>({
                ...x,
                subscriber: x.subscriber.id,
                createdAtTimestamp: Number(x.createdAtTimestamp),
                createdAtBlockNumber: Number(x.createdAtBlockNumber),
                updatedAtTimestamp: Number(x.updatedAtTimestamp),
                updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
                index: {
                    ...x.index,
                    token: {
                        ...x.index.token,
                        createdAtTimestamp: Number(
                            x.index.token.createdAtTimestamp
                        ),
                        createdAtBlockNumber: Number(
                            x.index.token.createdAtBlockNumber
                        ),
                    },
                },
            })
        );

        return createPagedResult<IIndexSubscription>(mappedResult, paging);
    };

    listStreams = async (
        filter: IStreamRequestFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IStream>> => {
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateStreamRequest(filter);

        const response = await this.subgraphClient.request<
            GetStreamsQuery,
            GetStreamsQueryVariables
        >(GetStreamsDocument, {
            where: {
                sender: filter.sender?.toLowerCase(),
                receiver: filter.receiver?.toLowerCase(),
                token: filter.token?.toLowerCase(),
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const mappedResult = response.result.map((x) =>
            typeGuard<IStream>({
                ...x,
                sender: x.sender.id,
                receiver: x.receiver.id,
                createdAtTimestamp: Number(x.createdAtTimestamp),
                createdAtBlockNumber: Number(x.createdAtBlockNumber),
                updatedAtTimestamp: Number(x.updatedAtTimestamp),
                updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
                token: {
                    ...x.token,
                    createdAtTimestamp: Number(x.token.createdAtTimestamp),
                    createdAtBlockNumber: Number(x.token.createdAtBlockNumber),
                },
                flowUpdatedEvents: x.flowUpdatedEvents.map((y) => ({
                    ...y,
                    blockNumber: Number(y.blockNumber),
                    timestamp: Number(y.timestamp),
                })),
            })
        );

        return createPagedResult<IStream>(mappedResult, paging);
    };

    listUserInteractedSuperTokens = async (
        filter: IAccountTokenSnapshotFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<ILightAccountTokenSnapshot>> => {
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateAccountTokenSnapshotRequest(filter);

        const response = await this.subgraphClient.request<
            GetAccountTokenSnapshotsQuery,
            GetAccountTokenSnapshotsQueryVariables
        >(GetAccountTokenSnapshotsDocument, {
            where: {
                account: filter.account?.toLowerCase(),
                token: filter.token?.toLowerCase(),
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const mappedResult = response.result.map((x) =>
            typeGuard<ILightAccountTokenSnapshot>({
                ...x,
                account: x.account.id,
                updatedAtTimestamp: Number(x.updatedAtTimestamp),
                updatedAtBlockNumber: Number(x.updatedAtBlockNumber),
                token: {
                    ...x.token,
                    createdAtTimestamp: Number(x.token.createdAtTimestamp),
                    createdAtBlockNumber: Number(x.token.createdAtBlockNumber),
                },
            })
        );

        return createPagedResult<ILightAccountTokenSnapshot>(
            mappedResult,
            paging
        );
    };

    listEvents = async (
        filter: IEventFilter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<AllEvents>> => {
        if (this.options.dataMode === "WEB3_ONLY") {
            throw new SFError({
                type: "UNSUPPORTED_WEB_3_ONLY",
                customMessage: "This query is not supported in WEB3_ONLY mode.",
            });
        }

        validateEventRequest(filter);

        const response = await this.subgraphClient.request<
            GetAllEventsQuery,
            GetAllEventsQueryVariables
        >(GetAllEventsDocument, {
            where: {
                addresses_contains: filter.account
                    ? [filter.account?.toLowerCase()]
                    : undefined,
                timestamp_gte: filter.timestamp_gte?.toString(),
            },
            skip: paging.skip,
            first: paging.takePlusOne(),
        });
        return createPagedResult<AllEvents>(
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
        if (ms < 1000) throw Error("Let's not go crazy with the queries...");

        // TODO: Wait for answer before next query...

        // Account for the fact that Subgraph has lag and will insert events with the timestamp of the event from blockchain.
        const clockSkew = 25000;

        let nextUtcNow = new Date().getTime() - clockSkew;
        const intervalId = setInterval(async () => {
            const utcNow = nextUtcNow;
            nextUtcNow += ms;

            const subgraphTime = Math.floor(utcNow / 1000);
            const accountEvents: AccountEvents[] = await this.listEvents({
                account: account,
                timestamp_gte: subgraphTime,
            }).then((x) => x.data as AccountEvents[]); // TODO(KK): Any way to do it without unsafe cast?

            if (accountEvents.length) {
                callback(accountEvents, unsubscribe);
            }
        }, ms);

        const unsubscribe = () => {
            clearInterval(intervalId);
        };

        if (timeout) {
            setTimeout(() => {
                unsubscribe();
            }, timeout);
        }

        return unsubscribe;
    }
}

// Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
const typeGuard = <T>(obj: T) => obj;
