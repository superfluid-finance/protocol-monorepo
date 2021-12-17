export const chainIds = [
    3, // ROPSTEN
    4, // RINKEBY
    5, // GOERLI
    42, // KOVAN
    // 100, // XDAI //TODO(KK): No infura support
    137, // MATIC
    80001, // MUMBAI
];

export type Network = {
    name: string;
    chainId: number;
};

export const networks: Network[] = [
    {
        name: "ropsten",
        chainId: 3,
    },
    {
        name: "rinkeby",
        chainId: 4,
    },
    {
        name: "goerli",
        chainId: 5,
    },
    {
        name: "kovan",
        chainId: 42,
    },
    {
        name: "matic",
        chainId: 137,
    },
    {
        name: "mumbai",
        chainId: 80001,
    },
];

export const networksByName = new Map(
    networks.map((x) => [x.name.toLowerCase(), x])
);

export const networksByChainId = new Map(networks.map((x) => [x.chainId, x]));

export const findNetwork = (x: unknown): Network | undefined => {
    if (typeof x === "string") {
        return networksByName.get(x.toLowerCase());
    }
};
