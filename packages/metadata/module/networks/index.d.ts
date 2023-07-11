declare const _default: {
    networks: import("./list.js").NetworkMetaData[];
    mainnets: import("./list.js").NetworkMetaData[];
    testnets: import("./list.js").NetworkMetaData[];
    getNetworkByChainId: (
        chainId: number
    ) => import("./list.js").NetworkMetaData | undefined;
    getNetworkByName: (
        name: string
    ) => import("./list.js").NetworkMetaData | undefined;
    getNetworkByShortName: (
        shortName: string
    ) => import("./list.js").NetworkMetaData | undefined;
};

export default _default;
