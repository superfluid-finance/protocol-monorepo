import { ethers } from "ethers";
import { DataMode } from "./interfaces";
import {
    IIndex,
    IIndexRequestFilter,
    IIndexSubscription,
    IIndexSubscriptionRequestFilter,
    ILightAccountTokenSnapshot,
    IPaginatedResponse,
    IPaginateRequest,
    IStream,
    IStreamRequestFilter,
    ISubgraphResponse,
    ISuperToken,
} from "./interfaces";
import { getAccountTokenSnapshotsByAccountQuery } from "./queries/aggregateQueries";
import {
    getIndexesQuery,
    getIndexSubscriptionsQuery,
    getStreamsQuery,
    getSuperTokensQuery,
} from "./queries/holQueries";
import { createPaginationResult, subgraphRequest } from "./queryHelpers";
import {
    buildWhereForSubgraphQuery,
    defaultPaginateOptions,
    normalizeAddressForSubgraph,
} from "./utils";
import {
    handleValidatePaginate,
    validateIndexRequest,
    validateIndexSubscriptionRequest,
    validateStreamRequest,
} from "./validation";

export interface IQueryOptions {
    readonly customSubgraphQueriesEndpoint: string;
    readonly dataMode: DataMode;
}

export default class Query {
    options: IQueryOptions;

    constructor(options: IQueryOptions) {
        this.options = options;
    }

    custom = async <T>(
        query: string,
        variables?: { [key: string]: any }
    ): Promise<ISubgraphResponse<T>> => {
        return await subgraphRequest<ISubgraphResponse<T>>(
            this.options.customSubgraphQueriesEndpoint,
            query,
            variables
        );
    };

    listAllSuperTokens = async (): Promise<
        ISubgraphResponse<ISuperToken[]>
    > => {
        return this.custom<ISuperToken[]>(getSuperTokensQuery);
    };

    listIndexes = async (
        filter: IIndexRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IIndex[]>> => {
        if (!validateIndexRequest(filter)) {
            throw new Error(
                "Invalid filter object - " +
                    JSON.stringify(validateIndexRequest.errors)
            );
        }
        handleValidatePaginate(paginateOptions);

        const where = buildWhereForSubgraphQuery(filter);
        const options = defaultPaginateOptions(paginateOptions);
        const result = await this.custom<IIndex[]>(
            getIndexesQuery(where, {
                ...options,
                first: options.first + 1,
            })
        );

        return createPaginationResult(result.response, options);
    };

    listIndexSubscriptions = async (
        filter: IIndexSubscriptionRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IIndexSubscription[]>> => {
        if (!validateIndexSubscriptionRequest(filter)) {
            throw new Error(
                "Invalid filter object - " +
                    JSON.stringify(validateIndexSubscriptionRequest.errors)
            );
        }
        handleValidatePaginate(paginateOptions);

        const where = buildWhereForSubgraphQuery(filter);
        const options = defaultPaginateOptions(paginateOptions);
        const result = await this.custom<IIndexSubscription[]>(
            getIndexSubscriptionsQuery(where, {
                ...options,
                first: options.first + 1,
            })
        );

        return createPaginationResult(result.response, options);
    };

    listStreams = async (
        filter: IStreamRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IStream[]>> => {
        if (!validateStreamRequest(filter)) {
            throw new Error(
                "Invalid filter object - " +
                    JSON.stringify(validateStreamRequest.errors)
            );
        }
        handleValidatePaginate(paginateOptions);

        const where = buildWhereForSubgraphQuery(filter);
        const options = defaultPaginateOptions(paginateOptions);
        const result = await this.custom<IStream[]>(
            getStreamsQuery(where, {
                ...options,
                first: options.first + 1,
            })
        );
        return createPaginationResult(result.response, options);
    };

    listUserInteractedSuperTokens = async (
        account: string
    ): Promise<ISubgraphResponse<ILightAccountTokenSnapshot[]>> => {
        const isValidAddress = ethers.utils.isAddress(account);
        if (isValidAddress === false) {
            throw new Error("The address you have entered is invalid.");
        }
        const normalizedAddress = normalizeAddressForSubgraph(account);

        return this.custom<ILightAccountTokenSnapshot[]>(
            getAccountTokenSnapshotsByAccountQuery,
            { account: normalizedAddress }
        );
    };
}
