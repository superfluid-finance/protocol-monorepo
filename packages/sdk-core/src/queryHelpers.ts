import { request } from "graphql-request";
import { IPaginatedResponse, IPaginateResponse } from "./interfaces";

export const subgraphRequest = async <T>(
    endpoint: string,
    query: string,
    variables?: { [key: string]: any }
): Promise<T> => {
    try {
        const response = await request<T>(endpoint, query, variables);
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
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
