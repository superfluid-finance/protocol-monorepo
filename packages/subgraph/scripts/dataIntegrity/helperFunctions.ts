import request, {gql} from "graphql-request";
import {toBN} from "../../test/helpers/helpers";
import {IMeta} from "../../test/interfaces";
import {TypedEvent} from "../../typechain/common";
import {IBaseEntity} from "../interfaces";

export const subgraphRequest = async <T>(
    query: string,
    subgraphEndpoint: string,
    variables?: {[key: string]: any}
): Promise<T> => {
    try {
        const response = await request<T>(subgraphEndpoint, query, variables);
        return response;
    } catch (err) {
        throw new Error(
            `Failed call to subgraph with query ${query} and error ${err}`
        );
    }
};

export const printProgress = (i: number, total: number, entityText: string) => {
    if ((i + 1) % 100 === 0) {
        console.log(`${i + 1}/${total} ${entityText} validated.`);
    }
};

export const getMostRecentIndexedBlockNumber = async (
    subgraphEndpoint: string
) => {
    const query = gql`
        query {
            _meta {
                block {
                    number
                }
            }
        }
    `;
    const data = await subgraphRequest<IMeta>(query, subgraphEndpoint);
    if (!data) return 0;

    return data._meta.block.number;
};

/**
 * @dev Chunks the promises as we don't want to exhaust CPU.
 * e.g. trying to do a promise for 5,000 items at once.
 */
export const chunkData = <T>(promises: T[], chunkLength: number) => {
    const chunksLength =
        promises.length < chunkLength
            ? promises.length
            : Math.ceil(promises.length / chunkLength);
    const batches = Array.apply(null, Array(chunksLength)).map((_x, i) =>
        promises.slice(i * chunkLength, chunkLength * (i + 1))
    );
    return batches;
};

export const validateEvents = <T extends TypedEvent, K>(
    eventName: string,
    onchainEvents: T[],
    subgraphEvents: K[],
    groupedEvents: _.Dictionary<T[]>
) => {
    // base case length equality check
    if (onchainEvents.length === subgraphEvents.length) {
        console.log(`${eventName} events length is the same.`);
    } else {
        throw new Error(`${eventName} events length are different.`);
    }

    for (let i = 0; i < subgraphEvents.length; i++) {
        const currentSubgraphEvent = subgraphEvents[i] as any; // TEMPORARY
        const id = currentSubgraphEvent.id.split(eventName + "-")[1];
        const currentOnChainEvent = groupedEvents[id][0];
        const keys = Object.keys(currentSubgraphEvent);
        if (currentOnChainEvent == null) {
            console.log("currentSubgraphEvent", currentSubgraphEvent);
            continue;
        }

        // validate the event properties
        for (let j = 0; j < keys.length; j++) {
            if (
                currentOnChainEvent.args[keys[j]] == null ||
                currentOnChainEvent.args[keys[j]] == undefined
            ) {
                continue;
            }
            // the properties are usually either string or BigInt
            if (typeof currentOnChainEvent.args[keys[j]] === "string") {
                if (
                    currentOnChainEvent.args[keys[j]].toLowerCase() !==
                    currentSubgraphEvent[keys[j]]
                ) {
                    console.log(
                        `${keys[j]} - Subgraph: ${
                            currentSubgraphEvent[keys[j]]
                        } \n On-chain data: ${currentOnChainEvent.args[
                            keys[j]
                        ].toLowerCase()}`
                    );
                    throw new Error(`${eventName} Event is not the same.`);
                }
            } else {
                if (
                    currentOnChainEvent.args[keys[j]].eq(
                        toBN(currentSubgraphEvent[keys[j]])
                    ) === false
                ) {
                    console.log(
                        `${keys[j]} - Subgraph: ${
                            currentSubgraphEvent[keys[j]]
                        } \n On-chain data: ${currentOnChainEvent.args[
                            keys[j]
                        ].toString()}`
                    );
                    throw new Error(`${eventName} Event is not the same.`);
                }
            }
        }

        printProgress(i, subgraphEvents.length, eventName + " events");
    }
    console.log(
        `\nAll ${subgraphEvents.length} ${eventName} events validated.\n`
    );
};

/**
 * @dev Gets all the results from the graph, we need this function
 * due to the 1,000 item limitation imposed by the
 */
export const getAllResults = async <T extends IBaseEntity>(
    query: string,
    endpoint: string,
    blockNumber: number,
    resultsPerPage: number,
    isUpdatedAt: boolean,
    isEvent: boolean = false,
    timestamp: number = 0,
    counter: number = 0
): Promise<T[]> => {
    const baseQuery = {blockNumber, first: resultsPerPage};
    const initialResults = await subgraphRequest<{response: T[]}>(
        query,
        endpoint,
        isEvent
            ? {
                  ...baseQuery,
                  timestamp,
              }
            : isUpdatedAt
            ? {
                  ...baseQuery,
                  updatedAt: timestamp,
              }
            : {
                  ...baseQuery,
                  createdAt: timestamp,
              }
    );

    console.log(
        counter * resultsPerPage +
            initialResults.response.length +
            " responses queried."
    );
    counter++;

    if (initialResults.response.length < resultsPerPage) {
        return initialResults.response;
    }

    let newTimestamp = isEvent
        ? initialResults.response[initialResults.response.length - 1].timestamp
        : isUpdatedAt
        ? initialResults.response[initialResults.response.length - 1]
              .updatedAtTimestamp
        : initialResults.response[initialResults.response.length - 1]
              .createdAtTimestamp;

    const responses = [
        ...initialResults.response,
        ...((await getAllResults(
            query,
            endpoint,
            blockNumber,
            resultsPerPage,
            isUpdatedAt,
            isEvent,
            Number(newTimestamp),
            counter
        )) as T[]),
    ];
    return responses;
};
