import { request } from "graphql-request";
import { handleError } from "./errorHelper";
import { IPaginatedResponse, IPaginateResponse } from "./interfaces";

export const subgraphRequest = async <T, S>(
    endpoint: string,
    query: string,
    variables?: S
): Promise<T> => {
    try {
        const response = await request<T, S>(endpoint, query, variables);
        return response;
    } catch (err) {
        return handleError(
            "SUBGRAPH_ERROR",
            `Failed call to subgraph with query ${query}`,
            JSON.stringify(err)
        );
    }
};

export const createPaginationResult = <T>(
    responsePlusOne: T[],
    options: IPaginateResponse
): IPaginatedResponse<T[]> => {
    const { first, skip } = options;
    let hasNextPage = responsePlusOne.length - 1 === first;
    let results = hasNextPage
        ? responsePlusOne.slice(0, responsePlusOne.length - 1)
        : responsePlusOne;

    return {
        hasNextPage,
        response: results,
        first,
        skip,
    };
};
