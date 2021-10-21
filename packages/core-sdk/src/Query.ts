import ethers from "ethers";
import {
    ILightAccountTokenSnapshot,
    IStream,
    ISubgraphResponse,
    ISuperToken,
} from "./interfaces";
import { getAccountTokenSnapshotsByAccount } from "./queries/aggregateQueries";
import { getStreams, getSuperTokens } from "./queries/holQueries";
import { subgraphRequest } from "./queryHelpers";
import { normalizeAddressForSubgraph } from "./utils";

export default class Query {
    endpoint: string;

    constructor(endpoint: string) {
        this.endpoint = endpoint;
    }

    custom = async <T>(
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
        return this.custom<ISuperToken[]>(getSuperTokens);
    };

    listStreams = async ({
        sender,
        receiver,
        token,
    }: {
        sender?: string;
        receiver?: string;
        token?: string;
    }): Promise<ISubgraphResponse<IStream[]>> => {
        const normalizedSender = normalizeAddressForSubgraph(sender || "");
        const normalizedReceiver = normalizeAddressForSubgraph(receiver || "");
        const normalizedToken = normalizeAddressForSubgraph(token || "");
        return this.custom<IStream[]>(getStreams, {
            sender: normalizedSender,
            receiver: normalizedReceiver,
            token: normalizedToken,
        });
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
            getAccountTokenSnapshotsByAccount,
            { account: normalizedAddress }
        );
    };
}
