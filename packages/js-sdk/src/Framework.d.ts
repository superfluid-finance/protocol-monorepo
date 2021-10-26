import type Web3 from "web3";
import type { ConstantFlowAgreementV1Helper } from "./ConstantFlowAgreementV1Helper";
import type { InstantDistributionAgreementV1Helper } from "./InstantDistributionAgreementV1Helper";
import type { GasMeter, Record } from "./utils/gasMetering/gasMetering";
import LoadContracts from "./loadContracts";
import Config from "./getConfig";
import type { User } from "./User";
import type { Utils } from "./Utils";
import type { Web3Provider } from "@ethersproject/providers";

declare type GasReportTypeOptions = 'JSON' | 'HTML' | 'TENDERLY';

export interface Agreements {
    cfa?: ConstantFlowAgreementV1Helper,
    ida?: InstantDistributionAgreementV1Helper
}

export interface FrameworkOptions {
    version?: string,
    isTruffle?: boolean,
    web3?: Web3,
    ethers?: Web3Provider,
    gasReportType?: GasReportTypeOptions,
    additionalContracts?: string[],
    tokens?: string[],
    loadSuperNativeToken?: boolean,
    contractLoader?: LoadContracts.ContractLoader,
    resolverAddress?: string,
}
export declare class Framework {

    constructor(options: FrameworkOptions);

    _options: FrameworkOptions;
    version: string;
    web3: Web3;
    ethers: Web3Provider;
    _gasReportType: GasReportTypeOptions;
    config: Config.NetworkConfig;
    contracts: Promise<LoadContracts.LoadedContract[]> | undefined;
    resolver: LoadContracts.LoadedContract;
    host: LoadContracts.LoadedContract;
    agreements: Agreements;
    tokens: { [key: string]: any };
    superTokens: { [key: string]: any };
    cfa: ConstantFlowAgreementV1Helper | undefined;
    ida: InstantDistributionAgreementV1Helper | undefined;
    utils: Utils | undefined;
    _gasMetering: GasMeter | undefined;

    initialize(): Promise<void>;
    isSuperTokenListed(superTokenKey: string): Promise<boolean>;
    loadToken(tokenKey: string): Promise<void>;
    createERC20Wrapper(tokenInfo: any,
        { superTokenSymbol, superTokenName, from, upgradability }: {
            superTokenSymbol?: string,
            superTokenName?: string,
            from?: string,
            upgradability?: string
        }
    ): Promise<any>;
    user({ address, token, options }: {
        address: string;
        token: string;
        options?: any;
    }): User;
    batchCall(calls: any): any;
    subgraphQuery(query: string): Promise<any>;
    getPastEvents(contract: any, eventName: any, filter: any, options: any): Promise<any>;
    _pushTxForGasReport(tx: Record, actionName: string): void;
    generateGasReport(name: string): void;
}
