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

export type { Record } from './utils/gasMetering/gasMetering';

export type {
    GasMeterHTMLReporter,
    GasMeterJSONReporter
} from './utils/gasMetering/gasReporter';

export type {
    getErrorResponse,
    getMissingArgumentError,
    getBatchCallHelpText
} from './utils/error';

export type {
    validateAddress,
    isAddress,
    getCleanAddress,
    completeTransaction
} from './utils/general';

export type { Agreement, batchCall } from './batchCall';

export type {
    Flow,
    ListFlowsType
} from './ConstantFlowAgreementV1Helper';

export type {
    Agreements,
    FrameworkOptions,
} from './Framework';

export type { NetworkConfig } from './getConfig';

export type { Subscription } from './InstantDistributionAgreementV1Helper';

export type {
    EthersWithSigner,
    LoadedContract,
    AbiContainer,
    ContractLoader
} from './loadContracts';

export type { DetailsType } from './User';