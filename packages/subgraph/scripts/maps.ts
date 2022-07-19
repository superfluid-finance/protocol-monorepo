import ropstenAddresses from "../config/ropsten.json";
import rinkebyAddresses from "../config/rinkeby.json";
import goerliAddresses from "../config/goerli.json";
import kovanAddresses from "../config/kovan.json";
import optimismKovanAddresses from "../config/optimism-kovan.json";
import optimismMainnetAddresses from "../config/optimism-mainnet.json";
import xdaiAddresses from "../config/xdai.json";
import maticAddresses from "../config/matic.json";
import avalancheFujiAddresses from "../config/avalanche-fuji.json";
import avalancheCAddresses from "../config/avalanche-c.json";
import mumbaiAddresses from "../config/mumbai.json";
import arbitrumRinkebyAddresses from "../config/arbitrum-rinkeby.json";
import arbitrumOneAddresses from "../config/arbitrum-one.json";

export const chainIdToData = new Map([
    [
        3,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten",
            name: "ropsten",
            addresses: ropstenAddresses,
        },
    ],
    [
        4,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby",
            name: "rinkeby",
            addresses: rinkebyAddresses,
        },
    ],
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
        42,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan",
            name: "kovan",
            addresses: kovanAddresses,
        },
    ],
    [
        69,
        {
            subgraphAPIEndpoint:
                "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-optimism-kovan",
            name: "optimism-kovan",
            addresses: optimismKovanAddresses,
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
            addresses: arbitrumRinkebyAddresses,
        },
    ],
]);
