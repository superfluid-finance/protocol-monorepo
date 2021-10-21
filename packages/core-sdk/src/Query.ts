import ethers from "ethers";
import {
    ILightAccountTokenSnapshot,
    ISubgraphResponse,
    ISuperToken,
} from "./interfaces";
import { getAccountTokenSnapshotsByAccount } from "./queries/aggregateQueries";
import { getSuperTokens } from "./queries/holQueries";
import { subgraphRequest } from "./queryHelpers";
import { normalizeAddressForSubgraph } from "./utils";

export default class Query {
    endpoint: string;

    constructor(endpoint: string) {
        this.endpoint = endpoint;
    }

    query = async <T>(
        query: string,
        variables?: { [key: string]: any }
    ): Promise<ISubgraphResponse<T>> => {
        return await subgraphRequest<ISubgraphResponse<T>>(
            this.endpoint,
            query,
            variables
        );
    };

    listAllSuperTokens = async (): Promise<
        ISubgraphResponse<ISuperToken[]>
    > => {
        return this.query<ISuperToken[]>(getSuperTokens);
    };

    listUserInteractedSuperTokens = async (
        account: string
    ): Promise<ISubgraphResponse<ILightAccountTokenSnapshot[]>> => {
        const isValidAddress = ethers.utils.isAddress(account);
        if (isValidAddress === false) {
            throw new Error("The address you have entered is invalid.");
        }
        const normalizedAddress = normalizeAddressForSubgraph(account);

        return this.query<ILightAccountTokenSnapshot[]>(
            getAccountTokenSnapshotsByAccount,
            { account: normalizedAddress }
        );
    };
}
