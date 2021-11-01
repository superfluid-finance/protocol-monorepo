import { ValidateFunction } from "ajv";
import { handleError } from "./errorHelper";
import {
    DataMode,
    IAccountTokenSnapshotFilter,
    IIndex,
    IIndexRequestFilter,
    IIndexSubscription,
    IIndexSubscriptionRequestFilter,
    ILightAccountTokenSnapshot,
    IPaginateResponse,
    IPaginatedResponse,
    IPaginateRequest,
    IStream,
    IStreamRequestFilter,
    ISubgraphResponse,
    ISuperToken,
    ISuperTokenRequestFilter,
} from "./interfaces";
import { getAccountTokenSnapshotsQuery } from "./queries/aggregateQueries";
import {
    getIndexesQuery,
    getIndexSubscriptionsQuery,
    getStreamsQuery,
    getSuperTokensQuery,
} from "./queries/holQueries";
import { createPaginationResult, subgraphRequest } from "./queryHelpers";
import { buildWhereForSubgraphQuery, defaultPaginateOptions } from "./utils";
import {
    handleValidatePaginate,
    validateAccountTokenSnapshotRequest,
    validateIndexRequest,
    validateIndexSubscriptionRequest,
    validateStreamRequest,
    validateSuperTokenRequest,
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

    customQuery = async <T, S>(
        query: string,
        variables?: S
    ): Promise<ISubgraphResponse<T>> => {
        return await subgraphRequest<ISubgraphResponse<T>, S>(
            this.options.customSubgraphQueriesEndpoint,
            query,
            variables
        );
    };

    paginatedQuery = async <T, S, U>(
        filter: T,
        paginateOptions: IPaginateRequest,
        validateFunction: ValidateFunction<T>,
        getQueryFunction: (
            where: string,
            paginateOptions: IPaginateResponse
        ) => string
    ): Promise<IPaginatedResponse<S[]>> => {
        if (!validateFunction(filter)) {
            handleError(
                "INVALID_OBJECT",
                "Invalid Filter Object",
                JSON.stringify(validateFunction.errors)
            );
        }
        handleValidatePaginate(paginateOptions);

        const where = buildWhereForSubgraphQuery(filter);
        const options = defaultPaginateOptions(paginateOptions);
        const result = await this.customQuery<S[], U>(
            getQueryFunction(where, { ...options, first: options.first + 1 })
        );
        return createPaginationResult(result.response, options);
    };

    listAllSuperTokens = async (
        filter: ISuperTokenRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<ISuperToken[]>> => {
        return this.paginatedQuery(
            filter,
            paginateOptions,
            validateSuperTokenRequest,
            getSuperTokensQuery
        );
    };

    listIndexes = async (
        filter: IIndexRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IIndex[]>> => {
        return this.paginatedQuery(
            filter,
            paginateOptions,
            validateIndexRequest,
            getIndexesQuery
        );
    };

    listIndexSubscriptions = async (
        filter: IIndexSubscriptionRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IIndexSubscription[]>> => {
        return this.paginatedQuery(
            filter,
            paginateOptions,
            validateIndexSubscriptionRequest,
            getIndexSubscriptionsQuery
        );
    };

    listStreams = async (
        filter: IStreamRequestFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<IStream[]>> => {
        return this.paginatedQuery(
            filter,
            paginateOptions,
            validateStreamRequest,
            getStreamsQuery
        );
    };

    listUserInteractedSuperTokens = async (
        filter: IAccountTokenSnapshotFilter,
        paginateOptions: IPaginateRequest
    ): Promise<IPaginatedResponse<ILightAccountTokenSnapshot[]>> => {
        return this.paginatedQuery(
            filter,
            paginateOptions,
            validateAccountTokenSnapshotRequest,
            getAccountTokenSnapshotsQuery
        );
    };
}
