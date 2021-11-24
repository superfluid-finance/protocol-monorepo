import { IResolverData } from "./interfaces";

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

export const chainIds = [
    3, // ROPSTEN
    4, // RINKEBY
    5, // GOERLI
    42, // KOVAN
    100, // XDAI
    137, // MATIC
    80001, // MUMBAI
];

export const networkNames: string[] = [
    "ropsten",
    "rinkeby",
    "goerli",
    "kovan",
    "xdai",
    "matic",
    "mumbai",
];

export const chainIdToDataMap = new Map<number, IResolverData>([
    [
        3,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
            networkName: "ropsten",
            resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
        },
    ],
    [
        4,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby",
            networkName: "rinkeby",
            resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A",
        },
    ],
    [
        5,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli",
            networkName: "goerli",
            resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E",
        },
    ],
    [
        42,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan",
            networkName: "kovan",
            resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B",
        },
    ],
    [
        100,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai",
            networkName: "xdai",
            resolverAddress: "0xD2009765189164b495c110D61e4D301729079911",
        },
    ],
    [
        137,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic",
            networkName: "matic",
            resolverAddress: "0xE0cc76334405EE8b39213E620587d815967af39C",
        },
    ],
    [
        80001,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai",
            networkName: "mumbai",
            resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3",
        },
    ],
]);

export const networkNameToChainIdMap = new Map<string, number>([
    ["ropsten", 3],
    ["rinkeby", 4],
    ["goerli", 5],
    ["kovan", 42],
    ["xdai", 100],
    ["matic", 137],
    ["mumbai", 80001],
]);
