import ethers from "ethers";
import Framework from "./Framework";
import {
    ILightAccountTokenSnapshot,
    IStream,
    IStreamRequest,
    ISubgraphResponse,
    ISuperToken,
} from "./interfaces";
import { getAccountTokenSnapshotsByAccount } from "./queries/aggregateQueries";
import { getStreams, getSuperTokens } from "./queries/holQueries";
import { subgraphRequest } from "./queryHelpers";
import {
    buildWhereForSubgraphQuery,
    normalizeAddressForSubgraph,
} from "./utils";

export default class Query {
    framework: Framework;

    constructor(framework: Framework) {
        this.framework = framework;
    }

    custom = async <T>(
        query: string,
        variables?: { [key: string]: any }
    ): Promise<ISubgraphResponse<T>> => {
        return await subgraphRequest<ISubgraphResponse<T>>(
            this.framework.options.customSubgraphQueriesEndpoint,
            query,
            variables
        );
    };

    listAllSuperTokens = async (): Promise<
        ISubgraphResponse<ISuperToken[]>
    > => {
        return this.custom<ISuperToken[]>(getSuperTokens);
    };

    listStreams = async (
        data: IStreamRequest
    ): Promise<ISubgraphResponse<IStream[]>> => {
        const where = buildWhereForSubgraphQuery(data);
        return this.custom<IStream[]>(getStreams(where));
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
