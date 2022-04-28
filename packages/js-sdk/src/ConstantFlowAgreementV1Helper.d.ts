import type { Transaction } from "web3-core";
import type { Framework } from "./Framework";
import type { LoadedContract } from "./loadContracts";
import type BN from 'bn.js';
import { GasOptions } from "./types/gasOptions";

// returned from listFlows
export interface FlowList {
  inFlows: {
    sender: string;
    receiver: string;
    flowRate: string;
  }[];
  outFlows: {
    sender: string;
    receiver: string;
    flowRate: string;
  }[];
}

// from CFAv1 contract
// returned from _sanitizeflowInfo
// returned from getFlow and getAccountFlowInfo
export interface FlowInfo {
  timestamp: Date;
  flowRate: string;
  deposit: string;
  owedDeposit: string;
}

// Same interface used for create and update flow
export interface CreateFlowOptions {
  superToken: string;
  sender: string;
  receiver: string;
  flowRate: string;
  userData?: string;
  onTransaction?: ()=>any;
  gasOptions?: GasOptions;
}
export type UpdateFlowOptions = CreateFlowOptions

// MUST include 'by' type, it's only diff between create and update
export interface DeleteFlowOptions {
  superToken: string;
  sender: string;
  receiver: string;
  flowRate: string;
  by?: string;
  userData?: string;
  onTransaction?: ()=>any;
  gasOptions?: GasOptions;
}

export interface GetFlowOptions {
  superToken: string;
  sender: string;
  receiver: string;
}

export interface GetNetFlowOptions {
  superToken: string;
  account: string;
}

export type GetAccountFlowInfoOptions = GetNetFlowOptions

export declare class ConstantFlowAgreementV1Helper {
  static _sanitizeflowInfo({
    timestamp,
    flowRate,
    deposit,
    owedDeposit,
  }: {
    timestamp: number | BN,
    flowRate: number | BN,
    deposit: number | BN,
    owedDeposit: number | BN
  }): FlowInfo;
  /**
   * @dev Create new helper class
   * @param {Framework} sf Superfluid Framework object
   *
   * NOTE: You should first call async function Framework.initialize to initialize the object.
   */
  constructor(sf: Framework);
  _sf: Framework;
  _cfa: LoadedContract;
  /**
   * @dev Create a new flow
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} sender sender of the flow
   * @param {addressParam} receiver receiver of the flow
   * @param {flowRateParam} flowRate the flowrate of the flow
   * @param {Buffer} userData the user data passed to the callbacks
   * @param {GasOptions} gasOptions pass network gas parameters
   * @param {Function} onTransaction function to be called when transaction hash has been generated
   * @return {Promise<Transaction>} web3 transaction object
   */
  createFlow({
    superToken,
    sender,
    receiver,
    flowRate,
    userData,
    onTransaction,
    gasOptions,
  }: CreateFlowOptions): Promise<Transaction>;
  /**
   * @dev Update a new flow with a new flow rate
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} sender sender of the flow
   * @param {addressParam} receiver receiver of the flow
   * @param {flowRateParam} flowRate the flowrate of the flow
   * @param {Buffer} userData the user data passed to the callbacks
   * @param {GasOptions} gasOptions override network gas parameters
   * @param {Function} onTransaction function to be called when transaction hash has been generated
   * @return {Promise<Transaction>} web3 transaction object
   */
  updateFlow({
    superToken,
    sender,
    receiver,
    flowRate,
    userData,
    onTransaction,
    gasOptions,
  }: UpdateFlowOptions): Promise<Transaction>;
  /**
   * @dev Delete a existing flow
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} sender sender of the flow
   * @param {addressParam} receiver receiver of the flow
   * @param {addressParam} by delete flow by a third party (liquidations)
   * @param {Buffer} userData the user data passed to the callbacks
   * @param {GasOptions} gasOptions override network gas parameters
   * @param {Function} onTransaction function to be called when transaction hash has been generated
   * @return {Promise<Transaction>} web3 transaction object
   */
  deleteFlow({
    superToken,
    sender,
    receiver,
    by,
    userData,
    onTransaction,
    gasOptions,
  }: DeleteFlowOptions): Promise<Transaction>;
  /**
   * @dev Get information of a existing flow
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} sender sender of the flow
   * @param {addressParam} receiver receiver of the flow
   * @return {Promise<object>} Informationo about the flow:
   *         - <Date> timestamp, time when the flow was last updated
   *         - <string> flowRate, flow rate of the flow
   *         - <string> deposit, deposit of the flow
   *         - <string> owedDeposit, owed deposit of the flow
   */
  getFlow({
    superToken,
    sender,
    receiver,
  }: GetFlowOptions): Promise<FlowInfo>;
  /**
   * @dev Get information of the net flow of an account
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} account the account for the query
   * @return {Promise<string>} Net flow rate of the account
   */
  getNetFlow({
    superToken,
    account,
  }: GetNetFlowOptions): Promise<string>;
  /**
   * @dev Get information of the net flow of an account
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} account the account for the query
   * @return {Promise<string>} Net flow rate of the account
   */
  getAccountFlowInfo({
    superToken,
    account,
  }: GetAccountFlowInfoOptions): Promise<FlowInfo>;

  getFlowEvents({
    token,
    receiver,
    sender,
  }: {
    token: string;
    receiver?: string;
    sender?: string;
  }): Promise<any[]>;
  /**
   * @dev List flows of the account
   * @param {tokenParam} superToken superToken for the flow
   * @param {addressParam} account the account for the query
   * @return {Promise<[]>}
   */
  listFlows({
    superToken,
    account,
    onlyInFlows,
    onlyOutFlows,
  }: {
    superToken: string;
    account: string;
    onlyInFlows?: boolean;
    onlyOutFlows?: boolean;
  }): Promise<FlowList>;
}
