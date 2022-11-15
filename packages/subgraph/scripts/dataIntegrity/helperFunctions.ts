import request, { gql } from "graphql-request";
import _ from "lodash";
import { toBN } from "../../test/helpers/helpers";
import { IMeta } from "../../test/interfaces";
import { TypedEvent } from "@superfluid-finance/sdk-core";
import { BaseEntity } from "./interfaces";

/**
 * @dev Extract type of key when using Object.keys.
 */
export const keys = Object.keys as <T>(o: T) => Extract<keyof T, string>[];

export const subgraphRequest = async <T>(
    query: string,
    subgraphEndpoint: string,
    variables?: { [key: string]: any }
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
    onChainData: Record<
        string,
        { events: T[]; groupedEvents: _.Dictionary<T[]> }
    >,
    subgraphEvents: K[]
) => {
    let errors = 0;

    const onchainEvents = onChainData[eventName].events;
    const groupedEvents = onChainData[eventName].groupedEvents;

    // base case length equality check
    if (onchainEvents.length === subgraphEvents.length) {
        console.log(`${eventName} events length is the same.`);
    } else {
        errors++;
        throw new Error(`${eventName} events length are different.`);
    }

    for (let i = 0; i < subgraphEvents.length; i++) {
        const currentSubgraphEvent = subgraphEvents[i] as any;
        const id = currentSubgraphEvent.id.split(eventName + "-")[1]; // this returns: txnId-logIndex
        const currentOnChainEvent = groupedEvents[id][0];
        const keys = Object.keys(currentSubgraphEvent);
        if (currentOnChainEvent == null) {
            console.error("Event exists on subgraph, but not on chain");
            console.log("currentSubgraphEvent", currentSubgraphEvent);
            continue;
        }

        // validate the event properties based on the returned subgraph event entity
        for (let j = 0; j < keys.length; j++) {
            // skip properties that don't exist on the events
            if (
                currentOnChainEvent.args[keys[j]] == null ||
                currentOnChainEvent.args[keys[j]] == undefined
            ) {
                continue;
            }

            // on-chain data is type string
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
                    errors++;
                    throw new Error(`${eventName} Event is not the same.`);
                }
                // on-chain data is type number
            } else if (typeof currentOnChainEvent.args[keys[j]] === "number") {
                if (
                    toBN(currentOnChainEvent.args[keys[j]]).eq(
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
                    errors++;
                    throw new Error(`${eventName} Event is not the same.`);
                }
                // on-chain data is type BigNumber
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
                    errors++;
                    throw new Error(`${eventName} Event is not the same.`);
                }
            }
        }

        printProgress(i, subgraphEvents.length, eventName + " events");
    }
    console.log(
        `\nAll ${subgraphEvents.length} ${eventName} events validated.\n`
    );
    return errors;
};

export const querySubgraphAndValidateEvents = async ({
    eventName,
    queryHelper,
    query,
    onChainEvents,
}: {
    eventName: string;
    queryHelper: QueryHelper;
    query: string;
    onChainEvents: any; // OnChainIDAEvents or OnChainCFAEvents
}) => {
    console.log(`\nQuerying all ${eventName} events via the Subgraph...`);
    const subgraphEvents = await queryHelper.getAllResults({
        query,
        isEvent: true,
    });
    const uniqueSubgraphEvents = _.uniqBy(subgraphEvents, (x) => x.id);
    console.log(
        `There are ${uniqueSubgraphEvents.length} ${eventName} events
            out of ${subgraphEvents.length} total ${eventName} events.`
    );

    return validateEvents(
        eventName,
        onChainEvents as any,
        uniqueSubgraphEvents
    );
};

interface IGetAllResultsQueryObject {
    query: string;
    isUpdatedAt?: boolean;
    isEvent?: boolean;
    timestamp?: number;
    counter?: number;
}

export class QueryHelper {
    readonly endpoint: string;
    readonly currentBlockNumber: number;
    readonly resultsPerPage: number;

    constructor(
        endpoint: string,
        currentBlockNumber: number,
        resultsPerPage: number
    ) {
        this.endpoint = endpoint;
        this.currentBlockNumber = currentBlockNumber;
        this.resultsPerPage = resultsPerPage;
    }

    /**
     * @dev Gets all the results given the 1000 item limitation imposed by the Graph.
     */
    getAllResults = async <T extends BaseEntity>({
        query,
        isUpdatedAt,
        isEvent = false,
        timestamp = 0,
        counter = 0,
    }: IGetAllResultsQueryObject): Promise<T[]> => {
        const baseQuery = {
            blockNumber: this.currentBlockNumber,
            first: this.resultsPerPage,
        };
        const initialResults = await subgraphRequest<{ response: T[] }>(
            query,
            this.endpoint,
            isEvent
                ? {
                      ...baseQuery,
                      timestamp,
                  }
                : // we have this distinction because our aggregate entities
                // don't have a createdAtTimestamp which would normally be
                // used to get the next results
                isUpdatedAt
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
            counter * this.resultsPerPage +
                initialResults.response.length +
                " responses queried."
        );
        counter++;

        if (initialResults.response.length < this.resultsPerPage) {
            return initialResults.response;
        }

        let newTimestamp = isEvent
            ? initialResults.response[initialResults.response.length - 1]
                  .timestamp
            : isUpdatedAt
            ? initialResults.response[initialResults.response.length - 1]
                  .updatedAtTimestamp
            : initialResults.response[initialResults.response.length - 1]
                  .createdAtTimestamp;

        const responses = [
            ...initialResults.response,
            ...((await this.getAllResults({
                query,
                isUpdatedAt,
                isEvent,
                timestamp: Number(newTimestamp),
                counter,
            })) as T[]),
        ];
        return responses;
    };
}

export const printTestOutcome = (
    success: boolean,
    successText: string,
    failureText: string
) => {
    if (success) {
        console.log(successText);
    } else {
        console.log(failureText);
    }
};
