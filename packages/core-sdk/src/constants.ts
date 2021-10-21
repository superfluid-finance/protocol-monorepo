import { ChainId, NetworkName } from ".";

export const chainIds = [
    ChainId.ROPSTEN,
    ChainId.RINKEBY,
    ChainId.GOERLI,
    ChainId.KOVAN,
    ChainId.XDAI,
    ChainId.MATIC,
    ChainId.MUMBAI,
];

export const networkNames = [
    NetworkName.ROPSTEN,
    NetworkName.RINKEBY,
    NetworkName.GOERLI,
    NetworkName.KOVAN,
    NetworkName.XDAI,
    NetworkName.MATIC,
    NetworkName.MUMBAI,
];

export const chainIdToDataMap = new Map([
    [
        ChainId.ROPSTEN,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
            name: NetworkName.ROPSTEN,
        },
    ],
    [
        ChainId.RINKEBY,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby",
            name: NetworkName.RINKEBY,
        },
    ],
    [
        ChainId.GOERLI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli",
            name: NetworkName.GOERLI,
        },
    ],
    [
        ChainId.KOVAN,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan",
            name: NetworkName.KOVAN,
        },
    ],
    [
        ChainId.XDAI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai",
            name: NetworkName.XDAI,
        },
    ],
    [
        ChainId.MATIC,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic",
            name: NetworkName.MATIC,
        },
    ],
    [
        ChainId.MUMBAI,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai",
            name: NetworkName.MUMBAI,
        },
    ],
]);

export const networkNameToChainIdMap = new Map([
    [NetworkName.ROPSTEN, ChainId.ROPSTEN],
    [NetworkName.RINKEBY, ChainId.RINKEBY],
    [NetworkName.GOERLI, ChainId.GOERLI],
    [NetworkName.KOVAN, ChainId.KOVAN],
    [NetworkName.XDAI, ChainId.XDAI],
    [NetworkName.MATIC, ChainId.MATIC],
    [NetworkName.MUMBAI, ChainId.MUMBAI],
]);
