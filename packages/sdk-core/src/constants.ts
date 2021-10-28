import { ChainId, NetworkName } from "./interfaces";

export const chainIds = [
    3, // ROPSTEN
    4, // RINKEBY
    5, // GOERLI
    42, // KOVAN
    100, // XDAI
    137, // MATIC
    80001, // MUMBAI
];

export const networkNames: NetworkName[] = [
    "ropsten",
    "rinkeby",
    "goerli",
    "kovan",
    "xdai",
    "matic",
    "mumbai",
];

export const chainIdToDataMap = new Map<
    ChainId,
    { subgraphAPIEndpoint: string; name: NetworkName }
>([
    [
        3,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
            name: "ropsten",
        },
    ],
    [
        4,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby",
            name: "rinkeby",
        },
    ],
    [
        5,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli",
            name: "goerli",
        },
    ],
    [
        42,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan",
            name: "kovan",
        },
    ],
    [
        100,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai",
            name: "xdai",
        },
    ],
    [
        137,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic",
            name: "matic",
        },
    ],
    [
        80001,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai",
            name: "mumbai",
        },
    ],
]);

export const networkNameToChainIdMap = new Map<NetworkName, ChainId>([
    ["ropsten", 3],
    ["rinkeby", 4],
    ["goerli", 5],
    ["kovan", 42],
    ["xdai", 100],
    ["matic", 137],
    ["mumbai", 80001],
]);

// TODO: refactor it to get it working w/ resolver similar to js-sdk
export const chainIdToAddresses = new Map([
    [
        3,
        {
            host: "0xF2B4E81ba39F5215Db2e05B2F66f482BB8e87FD2",
            cfaV1: "0xaD2F1f7cd663f6a15742675f975CcBD42bb23a88",
        },
    ],
    [
        4,
        {
            host: "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6",
            cfaV1: "0xF4C5310E51F6079F601a5fb7120bC72a70b96e2A",
        },
    ],
    [
        5,
        {
            host: "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9",
            cfaV1: "0xEd6BcbF6907D4feEEe8a8875543249bEa9D308E8",
        },
    ],
    [
        42,
        {
            host: "0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3",
            cfaV1: "0xECa8056809e7e8db04A8fF6e4E82cD889a46FE2F",
        },
    ],
    [
        100,
        {
            host: "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7",
            cfaV1: "0xEbdA4ceF883A7B12c4E669Ebc58927FBa8447C7D",
        },
    ],
    [
        137,
        {
            host: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
            cfaV1: "0x6EeE6060f715257b970700bc2656De21dEdF074C",
        },
    ],
    [
        80001,
        {
            host: "0xEB796bdb90fFA0f28255275e16936D25d3418603",
            cfaV1: "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873",
        },
    ],
]);

export const MONTHS_PER_YEAR = 12;
export const DAYS_PER_MONTH = 30;
export const HOURS_PER_DAY = 24;
export const MINUTES_PER_HOUR = 60;
export const SECONDS_PER_MINUTE = 60;
