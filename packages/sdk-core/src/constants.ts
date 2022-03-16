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
export const BASE_18 = 1e18;

export const ROPSTEN = "ropsten";
export const RINKEBY = "rinkeby";
export const GOERLI = "goerli";
export const KOVAN = "kovan";
export const MUMBAI = "mumbai";
export const OPTIMISM_KOVAN = "optimism-kovan";
export const ARBITRUM_RINKEBY = "arbitrum-rinkeby";
export const AVALANCHE_FUJI = "avalanche-fuji";
export const GNOSIS = "gnosis";
export const MATIC = "matic";
export const OPTIMISM_MAINNET = "optimism-mainnet";
export const ARBITRUM_ONE = "arbitrum-one";

export const chainIds = [
    3, // ROPSTEN
    4, // RINKEBY
    5, // GOERLI
    10, // OPTIMISM MAINNET
    42, // KOVAN
    69, // OPTIMISM KOVAN
    100, // GNOSIS
    137, // MATIC
    42161, // ARBITRUM ONE
    43113, // AVALANCHE FUJI
    80001, // MUMBAI
    421611, // ARBITRUM RINKEBY
];

export const networkNames: string[] = [
    ROPSTEN,
    RINKEBY,
    GOERLI,
    KOVAN,
    MUMBAI,
    OPTIMISM_KOVAN,
    ARBITRUM_RINKEBY,
    AVALANCHE_FUJI,
    GNOSIS,
    MATIC,
    OPTIMISM_MAINNET,
    ARBITRUM_ONE,
];

export const chainIdToResolverDataMap = new Map<number, IResolverData>([
    [
        3,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-ropsten",
            networkName: ROPSTEN,
            resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
        },
    ],
    [
        4,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-rinkeby",
            networkName: RINKEBY,
            resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A",
        },
    ],
    [
        5,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-goerli",
            networkName: GOERLI,
            resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E",
        },
    ],
    [
        10,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-mainnet",
            networkName: OPTIMISM_MAINNET,
            resolverAddress: "0x743B5f46BC86caF41bE4956d9275721E0531B186",
        },
    ],
    [
        42,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-kovan",
            networkName: KOVAN,
            resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B",
        },
    ],
    [
        69,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-kovan",
            networkName: OPTIMISM_KOVAN,
            resolverAddress: "0x218B65780615Ff134f9Ad810CB98839534D3C0D6",
        },
    ],
    [
        100,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-xdai",
            networkName: GNOSIS,
            resolverAddress: "0xD2009765189164b495c110D61e4D301729079911",
        },
    ],
    [
        137,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-matic",
            networkName: MATIC,
            resolverAddress: "0xE0cc76334405EE8b39213E620587d815967af39C",
        },
    ],
    [
        42161,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-one",
            networkName: ARBITRUM_ONE,
            resolverAddress: "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F",
        },
    ],
    [
        43113,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-avalanche-fuji",
            networkName: AVALANCHE_FUJI,
            resolverAddress: "0x141920741bC45b962B59c833cd849bA617F7ef38",
        },
    ],
    [
        80001,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-mumbai",
            networkName: MUMBAI,
            resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3",
        },
    ],
    [
        421611,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-arbitrum-rinkeby",
            networkName: ARBITRUM_RINKEBY,
            resolverAddress: "0xa2C0C70A1E922f5f060ec20EE3aF002C163b4567",
        },
    ],
]);

export const networkNameToChainIdMap = new Map<string, number>([
    [ROPSTEN, 3],
    [RINKEBY, 4],
    [GOERLI, 5],
    [KOVAN, 42],
    [OPTIMISM_KOVAN, 69],
    [AVALANCHE_FUJI, 43113],
    [MUMBAI, 80001],
    [ARBITRUM_RINKEBY, 421611],
    [GNOSIS, 100],
    [MATIC, 137],
    [OPTIMISM_MAINNET, 10],
    [ARBITRUM_ONE, 42161],
]);
