import goerliAddresses from "../config/goerli.json";
import optimismGoerliAddresses from "../config/optimism-goerli.json";
import optimismMainnetAddresses from "../config/optimism-mainnet.json";
import xdaiAddresses from "../config/xdai.json";
import maticAddresses from "../config/matic.json";
import avalancheFujiAddresses from "../config/avalanche-fuji.json";
import avalancheCAddresses from "../config/avalanche-c.json";
import mumbaiAddresses from "../config/mumbai.json";
import arbitrumGoerliAddresses from "../config/arbitrum-goerli.json";
import arbitrumOneAddresses from "../config/arbitrum-one.json";

export const chainIdToData = new Map([
    [
        5,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli",
            name: "goerli",
            addresses: goerliAddresses,
        },
    ],
    [
        10,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-optimism-mainnet",
            name: "optimism-mainnet",
            addresses: optimismMainnetAddresses,
        },
    ],
    [
        69,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-optimism-kovan",
            name: "optimism-kovan",
            addresses: optimismGoerliAddresses,
        },
    ],
    [
        100,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai",
            name: "xdai",
            addresses: xdaiAddresses,
        },
    ],
    [
        137,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic",
            name: "matic",
            addresses: maticAddresses,
        },
    ],
    [
        42161,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-arbitrum-one",
            name: "arbitrum-one",
            addresses: arbitrumOneAddresses,
        },
    ],
    [
        43113,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-avalanche-fuji",
            name: "avalanche-fuji",
            addresses: avalancheFujiAddresses,
        },
    ],
    [
        43114,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-avalanche-c",
            name: "avalanche-c",
            addresses: avalancheCAddresses,
        },
    ],
    [
        80001,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai",
            name: "mumbai",
            addresses: mumbaiAddresses,
        },
    ],
    [
        421611,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-arbitrum-rinkeby",
            name: "arbitrum-rinkeby",
            addresses: arbitrumGoerliAddresses,
        },
    ],
]);
