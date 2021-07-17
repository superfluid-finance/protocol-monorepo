export const getConfig: (chainId: any) => any;
export const loadContracts: ({ isTruffle, ethers, web3, from, additionalContracts, contractLoader, networkId, }: {
    isTruffle: any;
    ethers: any;
    web3: any;
    from: any;
    additionalContracts: any;
    contractLoader: any;
    networkId: any;
}) => Promise<{}>;
export const Framework: {
    new (options: any): import("./Framework");
};
