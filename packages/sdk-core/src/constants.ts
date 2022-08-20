import metadata from "@superfluid-finance/metadata";

import DefaultSubgraphReleaseTag from "./defaultSubgraphReleaseTag.json";
import { IResolverData } from "./interfaces";

/******* TIME CONSTANTS *******/
export const MONTHS_PER_YEAR = 12;
export const DAYS_PER_MONTH = 30;
export const DAYS_PER_WEEK = 7;
export const HOURS_PER_DAY = 24;
export const MINUTES_PER_HOUR = 60;
export const MINUTE_IN_SECONDS = 60;
export const HOUR_IN_SECONDS = MINUTE_IN_SECONDS * MINUTES_PER_HOUR;
export const DAY_IN_SECONDS = HOUR_IN_SECONDS * HOURS_PER_DAY;
export const WEEK_IN_SECONDS = DAY_IN_SECONDS * DAYS_PER_WEEK;
export const MONTH_IN_SECONDS = DAY_IN_SECONDS * DAYS_PER_MONTH;
export const YEAR_IN_SECONDS = MONTH_IN_SECONDS * MONTHS_PER_YEAR; // NOTE: Is 360 days (misses 5-6 days)
export const BASE_18 = 1e18;

export const chainIds = metadata.networks.map((x) => x.chainId);

/******* ACL AUTHORIZATION BIT OPERATIONS *******/
export const AUTHORIZE_FLOW_OPERATOR_CREATE = 1 << 0;
export const AUTHORIZE_FLOW_OPERATOR_UPDATE = 1 << 1;
export const AUTHORIZE_FLOW_OPERATOR_DELETE = 1 << 2;
export const AUTHORIZE_FULL_CONTROL =
    AUTHORIZE_FLOW_OPERATOR_CREATE |
    AUTHORIZE_FLOW_OPERATOR_UPDATE |
    AUTHORIZE_FLOW_OPERATOR_DELETE;

const subgraphReleaseTag =
    process.env.SUBGRAPH_RELEASE_TAG || DefaultSubgraphReleaseTag.value;

const baseUrl = `https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-${subgraphReleaseTag}`;

const getResolverData = (chainId: number): IResolverData => {
    const networkData = metadata.networks.find((x) => x.chainId === chainId);
    if (!networkData)
        return {
            subgraphAPIEndpoint: "",
            networkName: "",
            resolverAddress: "",
            nativeTokenSymbol: "",
        };
    return {
        subgraphAPIEndpoint: `${baseUrl}-${networkData?.shortName}`,
        networkName: networkData.name,
        resolverAddress: networkData.contractsV1.resolver,
        nativeTokenSymbol: networkData.nativeTokenSymbol,
    };
};

export const chainIdToResolverDataMap = new Map(
    metadata.networks.map((x) => [x.chainId, getResolverData(x.chainId)])
);

export const networkNameToChainIdMap = new Map(
    metadata.networks.map((x) => [x.name, x.chainId])
);
