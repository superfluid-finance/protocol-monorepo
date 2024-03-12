export const chainIds = [
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
