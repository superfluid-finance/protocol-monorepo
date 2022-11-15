import type { Contract as Web3Contract } from "web3-eth-contract";
import TruffleContract from "@truffle/contract";
import ethers, { Contract, ContractInterface } from "ethers";
import type Web3 from "web3";

type SuperfluidContractNames =
  | 'ERC20WithTokenInfo'
  | 'IConstantFlowAgreementV1'
  | 'IERC20'
  | 'IInstantDistributionAgreementV1'
  | 'IResolver'
  | 'ISETH'
  | 'ISuperAgreement'
  | 'ISuperToken'
  | 'ISuperTokenFactory'
  | 'ISuperfluid'
  | 'ISuperfluidGovernance'
  | 'SuperfluidLoader'
  | 'TestToken'
  | 'TokenInfo'
  | 'UUPSProxiable';

export type SuperfluidContractObject = {
  abi: ContractInterface;
  contractName: SuperfluidContractNames;
  at: (address: string) => Contract;
}

export type SuperfluidContracts = Record<SuperfluidContractNames, SuperfluidContractObject>
export type LoadedContract = Web3Contract | TruffleContract.Contract
export type AbiContainer = Pick<ethers.utils.Interface, "fragments">;
export type ContractLoader = (name: string) => AbiContainer;

declare function setTruffleContractDefaults(c: TruffleContract.Contract, networkId: number, from: string): void;

declare function defaultContractLoader(name: string): {contractName: string, abi: AbiContainer}

export declare function loadContracts({ isTruffle, web3, from, additionalContracts, contractLoader, networkId, }: {
    isTruffle: boolean;
    web3?: Web3;
    from: string;
    additionalContracts?: string[];
    contractLoader: ContractLoader;
    networkId: number;
}): Promise<SuperfluidContracts>;
