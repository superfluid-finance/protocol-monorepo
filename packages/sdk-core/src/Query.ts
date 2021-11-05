import {
    IAccountEventsFilter,
    IAccountTokenSnapshotFilter,
    IFlowUpdatedEvent,
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
import {DataMode} from "./types";
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
import {PagedResult, Paging} from "./pagination";
import {SubgraphClient} from "./subgraph/SubgraphClient";
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
import _ from "lodash";
import {
    GetFlowUpdatedEventsDocument,
    GetFlowUpdatedEventsQuery,
    GetFlowUpdatedEventsQueryVariables,
} from "./subgraph/queries/events/getFlowUpdatedEvents.generated";
import {FlowUpdatedEvent_Filter} from "./subgraph/schema.generated";
import {
    GetAccountEventsDocument,
    GetAccountEventsQuery,
    GetAccountEventsQueryVariables,
} from "./subgraph/queries/events/getAccountEvents.generated";
import {AccountEvents, TransferEvent, AllEvents} from "./events";
import {
    GetEventIdsDocument,
    GetEventIdsQuery,
    GetEventIdsQueryVariables,
} from "./subgraph/queries/events/getEventIds.generated";

import {
    GetAllEventsDocument,
    GetAllEventsQuery,
    GetAllEventsQueryVariables,
} from "./subgraph/queries/events/getAllEvents.generated";

export interface IQueryOptions {
    readonly customSubgraphQueriesEndpoint: string;
    readonly dataMode: DataMode;
}

function mapTransferEvent(x: {
    __typename: "TransferEvent";
    value: string;
    token: string;
    blockNumber: string;
    transactionHash: string;
    timestamp: string;
    from: { id: string };
    to: { id: string };
}) {
    return ({
        __typename: x.__typename,
        token: x.token,
        value: x.value,
        from: x.from.id,
        to: x.to.id,
        timestamp: x.timestamp,
        blockNumber: x.blockNumber,
        transactionHash: x.transactionHash,
    });
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

        const response = await this.subgraphClient.request<GetTokensQuery,
            GetTokensQueryVariables>(GetTokensDocument, {
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

        const response = await this.subgraphClient.request<GetIndexesQuery,
            GetIndexesQueryVariables>(GetIndexesDocument, {
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

        const response = await this.subgraphClient.request<GetIndexSubscriptionsQuery,
            GetIndexSubscriptionsQueryVariables>(GetIndexSubscriptionsDocument, {
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

        const response = await this.subgraphClient.request<GetStreamsQuery,
            GetStreamsQueryVariables>(GetStreamsDocument, {
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

        const response = await this.subgraphClient.request<GetAccountTokenSnapshotsQuery,
            GetAccountTokenSnapshotsQueryVariables>(GetAccountTokenSnapshotsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<ILightAccountTokenSnapshot>(
            response.result,
            paging
        );
    };

    // NOTE: Kaspar's unfinished stuff below!
    // TODO(KK): Don't use "FlowUpdatedEvent_Filter"

    listFlowUpdatedEvents = async (
        filter: FlowUpdatedEvent_Filter,
        paging: Paging = new Paging()
    ): Promise<PagedResult<IFlowUpdatedEvent>> => {
        const response = await this.subgraphClient.request<GetFlowUpdatedEventsQuery,
            GetFlowUpdatedEventsQueryVariables>(GetFlowUpdatedEventsDocument, {
            where: filter,
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        return new PagedResult<IFlowUpdatedEvent>(response.result, paging);
    };

    // TODO: Need a better way of querying account events
    listAccountEvents = async (filter: IAccountEventsFilter): Promise<AccountEvents[]> => {
        // TODO: validate filter

        const response = await this.subgraphClient.request<GetAccountEventsQuery,
            GetAccountEventsQueryVariables>(GetAccountEventsDocument, {
            accountBytes: filter.account,
            accountString: filter.account,
            timestamp_gte: filter.timestamp_gte,
        });

        const accountEvents: AccountEvents[] = [
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
            ...response.transferEvents_to.map<TransferEvent>(mapTransferEvent),
            ...response.transferEvents_from.map<TransferEvent>(mapTransferEvent),
            ...response.tokenDowngradedEvents,
            ...response.tokenUpgradedEvents,
            ...response.subscriptionApprovedEvents_subscriber,
            ...response.subscriptionApprovedEvents_publisher,
            ...response.subscriptionDistributionClaimedEvents_subscriber,
            ...response.subscriptionDistributionClaimedEvents_publisher,
            ...response.subscriptionRevokedEvents_subscriber,
            ...response.subscriptionRevokedEvents_publisher,
            ...response.subscriptionUnitsUpdatedEvents_subscriber,
            ...response.subscriptionUnitsUpdatedEvents_publisher,
        ];

        return accountEvents;
    }

    listAllEvents = async (
        paging: Paging = new Paging()
    ): Promise<PagedResult<AllEvents>> => {
        const getEventIdsQueryResponse = await this.subgraphClient.request<GetEventIdsQuery,
            GetEventIdsQueryVariables>(GetEventIdsDocument, {
            skip: paging.skip,
            first: paging.takePlusOne(),
        });

        const getAllEventsQueryResponse = await this.subgraphClient.request<GetAllEventsQuery,
            GetAllEventsQueryVariables>(GetAllEventsDocument, {
            ids: getEventIdsQueryResponse.events.map((x) => x.id),
        });

        const resultEvents: AllEvents[] = [
            ...getAllEventsQueryResponse.agreementClassRegisteredEvents,
            ...getAllEventsQueryResponse.agreementClassUpdatedEvents,
            ...getAllEventsQueryResponse.agreementLiquidatedByEvents,
            ...getAllEventsQueryResponse.appRegisteredEvents,
            ...getAllEventsQueryResponse.burnedEvents,
            ...getAllEventsQueryResponse.cfav1LiquidationPeriodChangedEvents,
            ...getAllEventsQueryResponse.configChangedEvents,
            ...getAllEventsQueryResponse.customSuperTokenCreatedEvents,
            ...getAllEventsQueryResponse.flowUpdatedEvents,
            ...getAllEventsQueryResponse.governanceReplacedEvents,
            ...getAllEventsQueryResponse.indexCreatedEvents,
            ...getAllEventsQueryResponse.indexDistributionClaimedEvents,
            ...getAllEventsQueryResponse.indexSubscribedEvents,
            ...getAllEventsQueryResponse.indexUnitsUpdatedEvents,
            ...getAllEventsQueryResponse.indexUnsubscribedEvents,
            ...getAllEventsQueryResponse.indexUpdatedEvents,
            ...getAllEventsQueryResponse.jailEvents,
            ...getAllEventsQueryResponse.mintedEvents,
            ...getAllEventsQueryResponse.rewardAddressChangedEvents,
            ...getAllEventsQueryResponse.roleAdminChangedEvents,
            ...getAllEventsQueryResponse.roleGrantedEvents,
            ...getAllEventsQueryResponse.roleRevokedEvents,
            ...getAllEventsQueryResponse.sentEvents,
            ...getAllEventsQueryResponse.subscriptionApprovedEvents,
            ...getAllEventsQueryResponse.subscriptionDistributionClaimedEvents,
            ...getAllEventsQueryResponse.subscriptionRevokedEvents,
            ...getAllEventsQueryResponse.subscriptionUnitsUpdatedEvents,
            ...getAllEventsQueryResponse.superTokenCreatedEvents,
            ...getAllEventsQueryResponse.superTokenFactoryUpdatedEvents,
            ...getAllEventsQueryResponse.superTokenLogicCreatedEvents,
            ...getAllEventsQueryResponse.superTokenLogicUpdatedEvents,
            ...getAllEventsQueryResponse.tokenDowngradedEvents,
            ...getAllEventsQueryResponse.transferEvents.map<TransferEvent>(
                (x) => ({
                    __typename: x.__typename,
                    token: x.token,
                    value: x.value,
                    from: x.from.id,
                    to: x.to.id,
                    timestamp: x.timestamp,
                    blockNumber: x.blockNumber,
                    transactionHash: x.transactionHash,
                })
            ),
            ...getAllEventsQueryResponse.trustedForwarderChangedEvents,
            ...getAllEventsQueryResponse.tokenUpgradedEvents,
        ];

        return new PagedResult<AllEvents>(resultEvents, paging);
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
        let nextUtcNow =  new Date().getTime() - timeSkew;
        const intervalId = setInterval(async () => {
            console.log("interval");
            const utcNow = nextUtcNow;
            nextUtcNow += ms;

            const subgraphTime = Math.floor(utcNow / 1000)
            const accountEvents: AccountEvents[] = await this.listAccountEvents({
                account: account,
                timestamp_gte: subgraphTime.toString()
            });

            if (accountEvents.length) {
                console.log('callback');
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
