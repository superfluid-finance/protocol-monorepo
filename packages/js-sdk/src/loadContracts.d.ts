import { ethers, Contract as EthersContract, utils } from "ethers"; 
import type { Contract as Web3Contract } from "web3-eth-contract";
import TruffleContract from "@truffle/contract";
import type Web3 from "web3";

export interface EthersWithSigner {
    getSigner(): () => ethers.Signer
}

export type LoadedContract = Web3Contract | TruffleContract.Contract | EthersContract;
export type AbiContainer = Pick<utils.Interface, "abi">;
export type ContractLoader = (name: string) => AbiContainer;

declare function setTruffleContractDefaults(c: TruffleContract.Contract, networkId: number, from: string): void;

declare function defaultContractLoader(name: string): {contractName: string, abi: AbiContainer}

interface AdaptedContractOpts {
    address: string;
    abi: AbiContainer;
    ethers: EthersWithSigner;
}
declare function getAdaptedContract({address, abi, ethers}: AdaptedContractOpts): EthersContract;

declare function loadContracts({ isTruffle, ethers, web3, from, additionalContracts, contractLoader, networkId, }: {
    isTruffle: boolean;
    ethers?: EthersWithSigner;
    web3?: Web3;
    from: string;
    additionalContracts?: string[];
    contractLoader: ContractLoader;
    networkId: number;
}): Promise<LoadedContract[]>;

export = loadContracts;