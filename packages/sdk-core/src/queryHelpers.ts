import { request } from "graphql-request";

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
