import ConstantFlowAgreementV1Helper from "./ConstantFlowAgreementV1Helper";
import InstantDistributionAgreementV1Helper from "./InstantDistributionAgreementV1Helper";
import GasMeter from "./utils/gasMetering/gasMetering";
import User from "./User";
import Utils from "./Utils";
import type Web3 from "web3";
import { Contract } from "ethers";
import TruffleContract from "@truffle/contract";

export declare type GasReportTypeOptions = 'JSON' | 'HTML' | 'TENDERLY';
export interface FrameworkOptions {
    version: string,
    ethers: any,
    web3: any,
    isTruffle: boolean,
    gasReportType: GasReportTypeOptions,
    additionalContracts: Array<any>,
    tokens: Array<string>,
    loadSuperNativeToken: boolean,
    contractLoader: any,
    resolverAddress: string,
}
declare class Framework {

    _options: FrameworkOptions;
    version: string;
    web3: Web3;
    ethers: any;
    _gasReportType: GasReportTypeOptions;

    constructor(options: FrameworkOptions);

    initialize(): Promise<any>;

    config: any;
    contracts: Promise<Contract[] | Web3.Eth.Contract[] | TruffleContract.Contract[]> | undefined;
    resolver: any;
    host: any;
    agreements: {} | undefined;
    tokens: {} | undefined;
    superTokens: {} | undefined;
    cfa: ConstantFlowAgreementV1Helper | undefined;
    ida: InstantDistributionAgreementV1Helper | undefined;
    utils: Utils | undefined;
    _gasMetering: GasMeter | undefined;

    isSuperTokenListed(superTokenKey: string): Promise<boolean>;
    loadToken(tokenKey: string): Promise<void>;
    createERC20Wrapper(tokenInfo: any, { superTokenSymbol, superTokenName, from, upgradability }?: string): Promise<any>;
    user({ address, token, options }: {
        address: string;
        token: string;
        options: any;
    }): User;
    batchCall(calls: any): any;
    _pushTxForGasReport(tx: any, actionName: string): void;
    generateGasReport(name: string): void;
}


export = Framework;