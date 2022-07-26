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

/******* NETWORK NAMES *******/
export const ETH_ROPSTEN = "eth-ropsten";
export const ETH_RINKEBY = "eth-rinkeby";
export const ETH_GOERLI = "eth-goerli";
export const ETH_KOVAN = "eth-kovan";

export const POLYGON_MAINNET = "polygon-mainnet";
export const POLYGON_MUMBAI = "polygon-mumbai";

export const XDAI_MAINNET = "xdai-mainnet";

export const OPTIMISM_MAINNET = "optimism-mainnet";
export const OPTIMISM_KOVAN = "optimism-kovan";

export const ARBITRUM_ONE = "arbitrum-one";
export const ARBITRUM_RINKEBY = "arbitrum-rinkeby";

export const AVALANCHE_C = "avalanche-c";
export const AVALANCHE_FUJI = "avalanche-fuji";

export const BSC_MAINNET = "bsc-mainnet";

/******* CHAIN IDS *******/
export const ETH_ROPSTEN_CHAIN_ID = 3;
export const ETH_RINKEBY_CHAIN_ID = 4;
export const ETH_GOERLI_CHAIN_ID = 5;
export const ETH_KOVAN_CHAIN_ID = 42;

export const GNOSIS_CHAIN_ID = 100;

export const MATIC_CHAIN_ID = 137;
export const MUMBAI_CHAIN_ID = 80001;

export const OPTIMISM_MAINNET_CHAIN_ID = 10;
export const OPTIMISM_KOVAN_CHAIN_ID = 69;

export const ARBITRUM_ONE_CHAIN_ID = 42161;
export const ARBITRUM_RINKEBY_CHAIN_ID = 421611;

export const AVALANCHE_FUJI_CHAIN_ID = 43113;
export const AVALANCHE_C_CHAIN_ID = 43114;

export const BSC_MAINNET_CHAIN_ID = 56;

export const chainIds = [
    ETH_ROPSTEN_CHAIN_ID, // ROPSTEN
    ETH_RINKEBY_CHAIN_ID, // RINKEBY
    ETH_GOERLI_CHAIN_ID, // GOERLI
    ETH_KOVAN_CHAIN_ID, // KOVAN

    MATIC_CHAIN_ID, // MATIC
    MUMBAI_CHAIN_ID, // MUMBAI

    GNOSIS_CHAIN_ID, // GNOSIS

    OPTIMISM_MAINNET_CHAIN_ID, // OPTIMISM MAINNET
    OPTIMISM_KOVAN_CHAIN_ID, // OPTIMISM KOVAN

    ARBITRUM_ONE_CHAIN_ID, // ARBITRUM ONE
    ARBITRUM_RINKEBY_CHAIN_ID, // ARBITRUM RINKEBY

    AVALANCHE_FUJI_CHAIN_ID, // AVALANCHE FUJI
    AVALANCHE_C_CHAIN_ID, // AVALANCHE C-CHAIN

    BSC_MAINNET_CHAIN_ID, // BNB MAINNET
];

/******* ACL AUTHORIZATION BIT OPERATIONS *******/
export const AUTHORIZE_FLOW_OPERATOR_CREATE = 1 << 0;
export const AUTHORIZE_FLOW_OPERATOR_UPDATE = 1 << 1;
export const AUTHORIZE_FLOW_OPERATOR_DELETE = 1 << 2;
export const AUTHORIZE_FULL_CONTROL =
    AUTHORIZE_FLOW_OPERATOR_CREATE |
    AUTHORIZE_FLOW_OPERATOR_UPDATE |
    AUTHORIZE_FLOW_OPERATOR_DELETE;

const subgraphReleaseTag =
    (globalThis.process && globalThis.process.env.SUBGRAPH_RELEASE_TAG) ||
    DefaultSubgraphReleaseTag.value;

const baseUrl = `https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-${subgraphReleaseTag}`;

export const chainIdToResolverDataMap = new Map<number, IResolverData>([
    [
        ETH_ROPSTEN_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-ropsten`,
            networkName: ETH_ROPSTEN,
            resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        ETH_RINKEBY_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-rinkeby`,
            networkName: ETH_RINKEBY,
            resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        ETH_GOERLI_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-goerli`,
            networkName: ETH_GOERLI,
            resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        OPTIMISM_MAINNET_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-optimism-mainnet`,
            networkName: OPTIMISM_MAINNET,
            resolverAddress: "0x743B5f46BC86caF41bE4956d9275721E0531B186",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        ETH_KOVAN_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-kovan`,
            networkName: ETH_KOVAN,
            resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        OPTIMISM_KOVAN_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-optimism-kovan`,
            networkName: OPTIMISM_KOVAN,
            resolverAddress: "0x218B65780615Ff134f9Ad810CB98839534D3C0D6",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        GNOSIS_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-xdai`,
            networkName: XDAI_MAINNET,
            resolverAddress: "0xD2009765189164b495c110D61e4D301729079911",
            nativeTokenSymbol: "xDAI",
        },
    ],
    [
        MATIC_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-matic`,
            networkName: POLYGON_MAINNET,
            resolverAddress: "0xE0cc76334405EE8b39213E620587d815967af39C",
            nativeTokenSymbol: "MATIC",
        },
    ],
    [
        ARBITRUM_ONE_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-arbitrum-one`,
            networkName: ARBITRUM_ONE,
            resolverAddress: "0x609b9d9d6Ee9C3200745A79B9d3398DBd63d509F",
            nativeTokenSymbol: "ETH",
        },
    ],
    [
        AVALANCHE_FUJI_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-avalanche-fuji`,
            networkName: AVALANCHE_FUJI,
            resolverAddress: "0x141920741bC45b962B59c833cd849bA617F7ef38",
            nativeTokenSymbol: "AVAX",
        },
    ],
    [
        AVALANCHE_C_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-avalanche-c`,
            networkName: AVALANCHE_C,
            resolverAddress: "0x24a3F04F70B7f07B9673EadD3e146391BcfEa5c1",
            nativeTokenSymbol: "AVAX",
        },
    ],
    [
        BSC_MAINNET_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-bsc-mainnet`,
            networkName: BSC_MAINNET,
            resolverAddress: "0x69604aA4e9e8BF44A73C680997205Edb03A92E41",
            nativeTokenSymbol: "BNB",
        },
    ],
    [
        MUMBAI_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-mumbai`,
            networkName: POLYGON_MUMBAI,
            resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3",
            nativeTokenSymbol: "MATIC",
        },
    ],
    [
        ARBITRUM_RINKEBY_CHAIN_ID,
        {
            subgraphAPIEndpoint: `${baseUrl}-arbitrum-rinkeby`,
            networkName: ARBITRUM_RINKEBY,
            resolverAddress: "0xa2C0C70A1E922f5f060ec20EE3aF002C163b4567",
            nativeTokenSymbol: "ETH",
        },
    ],
]);

export const networkNameToChainIdMap = new Map<string, number>([
    [ETH_ROPSTEN, ETH_ROPSTEN_CHAIN_ID],
    [ETH_RINKEBY, ETH_RINKEBY_CHAIN_ID],
    [ETH_GOERLI, ETH_GOERLI_CHAIN_ID],
    [ETH_KOVAN, ETH_KOVAN_CHAIN_ID],

    [XDAI_MAINNET, GNOSIS_CHAIN_ID],

    [POLYGON_MAINNET, MATIC_CHAIN_ID],
    [POLYGON_MUMBAI, MUMBAI_CHAIN_ID],

    [OPTIMISM_KOVAN, OPTIMISM_KOVAN_CHAIN_ID],
    [OPTIMISM_MAINNET, OPTIMISM_MAINNET_CHAIN_ID],

    [ARBITRUM_ONE, ARBITRUM_ONE_CHAIN_ID],
    [ARBITRUM_RINKEBY, ARBITRUM_RINKEBY_CHAIN_ID],

    [AVALANCHE_C, AVALANCHE_C_CHAIN_ID],
    [AVALANCHE_FUJI, AVALANCHE_FUJI_CHAIN_ID],

    [BSC_MAINNET, BSC_MAINNET_CHAIN_ID],
]);
