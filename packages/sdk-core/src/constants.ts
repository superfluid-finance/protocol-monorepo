import { ChainId } from "./index";
import { NetworkName } from "./interfaces";

export const chainIds = [
    ChainId.ROPSTEN,
    ChainId.RINKEBY,
    ChainId.GOERLI,
    ChainId.KOVAN,
    ChainId.XDAI,
    ChainId.MATIC,
    ChainId.MUMBAI,
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
        ChainId.ROPSTEN,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
            name: "ropsten",
        },
    ],
    [
        ChainId.RINKEBY,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby",
            name: "rinkeby",
        },
    ],
    [
        ChainId.GOERLI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli",
            name: "goerli",
        },
    ],
    [
        ChainId.KOVAN,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan",
            name: "kovan",
        },
    ],
    [
        ChainId.XDAI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai",
            name: "xdai",
        },
    ],
    [
        ChainId.MATIC,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic",
            name: "matic",
        },
    ],
    [
        ChainId.MUMBAI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai",
            name: "mumbai",
        },
    ],
]);

export const networkNameToChainIdMap = new Map<NetworkName, ChainId>([
    ["ropsten", ChainId.ROPSTEN],
    ["rinkeby", ChainId.RINKEBY],
    ["goerli", ChainId.GOERLI],
    ["kovan", ChainId.KOVAN],
    ["xdai", ChainId.XDAI],
    ["matic", ChainId.MATIC],
    ["mumbai", ChainId.MUMBAI],
]);

// TODO: refactor it to get it working w/ resolver similar to js-sdk
export const chainIdToAddresses = new Map([
    [
        ChainId.ROPSTEN,
        {
            host: "0xF2B4E81ba39F5215Db2e05B2F66f482BB8e87FD2",
            cfaV1: "0xaD2F1f7cd663f6a15742675f975CcBD42bb23a88",
        },
    ],
    [
        ChainId.RINKEBY,
        {
            host: "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6",
            cfaV1: "0xF4C5310E51F6079F601a5fb7120bC72a70b96e2A",
        },
    ],
    [
        ChainId.GOERLI,
        {
            host: "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9",
            cfaV1: "0xEd6BcbF6907D4feEEe8a8875543249bEa9D308E8",
        },
    ],
    [
        ChainId.KOVAN,
        {
            host: "0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3",
            cfaV1: "0xECa8056809e7e8db04A8fF6e4E82cD889a46FE2F",
        },
    ],
    [
        ChainId.XDAI,
        {
            host: "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7",
            cfaV1: "0xEbdA4ceF883A7B12c4E669Ebc58927FBa8447C7D",
        },
    ],
    [
        ChainId.MATIC,
        {
            host: "0x3E14dC1b13c488a8d5D310918780c983bD5982E7",
            cfaV1: "0x6EeE6060f715257b970700bc2656De21dEdF074C",
        },
    ],
    [
        ChainId.MUMBAI,
        {
            host: "0xEB796bdb90fFA0f28255275e16936D25d3418603",
            cfaV1: "0x49e565Ed1bdc17F3d220f72DF0857C26FA83F873",
        },
    ],
]);
