import { providers } from 'ethers';
type Network = {
    id: string;
    name: string;
    subgraphUrl: string;
    url: string;
    testnet: boolean;
    devSubgraphUrl: string;
};

type Networks = {
    [key: number]: Network;
};

export const networks: Networks = {
    3: {
        id: '3',
        name: 'Ropsten',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-ropsten',
        url: `https://ropsten.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: true,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-ropsten',
    },
    4: {
        id: '4',
        name: 'Rinkeby',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-rinkeby',
        url: `https://rinkeby.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: true,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-rinkeby',
    },
    5: {
        id: '5',
        name: 'Goerli',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-goerli',
        url: `https://goerli.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: true,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-goerli',
    },
    42: {
        id: '42',
        name: 'Kovan',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-kovan',
        url: `https://kovan.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: true,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-kovan',
    },
    100: {
        id: '100',
        name: 'xDAI',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-xdai',
        url: 'https://rpc.xdaichain.com/',
        testnet: false,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-xdai',
    },
    137: {
        id: '137',
        name: 'Matic',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-matic',
        url: `https://polygon-mainnet.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: false,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-matic',
    },
    8001: {
        id: '8001',
        name: 'Mumbai',
        subgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/superfluid-mumbai',
        url: `https://polygon-mumbai.infura.io/v3/${process.env.REACT_APP_INFURA_ID}`,
        testnet: true,
        devSubgraphUrl:
            'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-dev-mumbai',
    },
};

const nameToId: { [key: string]: string } = {
    ropsten: '3',
    rinkeby: '4',
    goerli: '5',
    kovan: '42',
    xdai: '100',
    matic: '137',
    mumbai: '8001',
};

export const getNetworkByChainId = (chainId: string): Network => {
    const network = networks[parseInt(chainId)];
    return network;
};

export const getChainIdByName = (name: string): Network | null => {
    const nameLower = name.toLowerCase();
    const chainId = nameToId[nameLower];
    if (chainId) {
        return getNetworkByChainId(chainId);
    }
    return null;
};

export const getProviderByChainName = async (
    chainName: string
): Promise<providers.BaseProvider> => {
    const network = getChainIdByName(chainName);
    return network
        ? providers.getDefaultProvider(network.url)
        : providers.getDefaultProvider(undefined);


};
